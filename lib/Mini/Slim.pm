package Mini::Slim;

use warnings;
use strict;
use 5.006;
use v5.10.0;

use IO::Select;
use IO::Socket::INET;
use Time::HiRes qw(tv_interval gettimeofday);
use blib;
use Mini::Slim::Util;
use base 'Mini::Slim::Util';

=head1 NAME

Mini::Slim - A truly "slim" replacement for slimserver/squeezecenter

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Serve music to squeezeboxes and similar devices.

    use Mini::Slim;

    Mini::Slim->new->server();


=head1 CONSTRUCTOR

=head2 new

Create a new server object.

=cut

sub new {
    my $package = shift;
    my @args = @_;
    my $musicdir = config('musicdir');
    foreach my $i (0..@args-1) {
        if(substr($args[$i], 0, length($musicdir)) eq $musicdir) {
            substr($args[$i], 0, length($musicdir), '');
        }
    }
    my $self = bless({
        alive => 1,
        sel   => IO::Select->new(),
        args  => \@args,
        work  => [],
    }, $package);
    return $self;
}


=head1 TOP LEVEL METHODS

=head2 server

Run the server.  This is the main function.

=cut

sub server {
    my $self = shift;
    $self->info("Starting server.\n");
    my $listener = IO::Socket::INET->new(
        Proto     => 'tcp',
        Listen    => 1,
        LocalAddr => config('bind-addr', '0.0.0.0'),
        LocalPort => config('bind-port', 3483),
        ReuseAddr => 1,
    );
    $$self{sel}->add($listener);
    while($$self{alive}) {
        my $sleep_interval = 5;
        if(scalar @{$$self{work}}) {
            $sleep_interval = 0.1;
            $self->do_work();
        }
        my @handles = $$self{sel}->can_read($sleep_interval);
        foreach my $handle (@handles) {
            if($handle == $listener) {
                $self->handle_listener_read($handle);
            } else {
                $self->handle_client_read($handle);
            }
        }
    }
}

sub do_work {
    my $self = shift;
    my @pending = @{$$self{work}};
    $$self{work} = [];
    while(scalar @pending) {
        my $work = shift @pending;
        $$work{args} = [] unless exists $$work{args};
        my $handler  = $$work{handler};
        my @args     = @{$$work{args}};
        &$handler(@args);
    }
}

sub schedule_work {
    my ($self, $handler, @args) = @_;
    push(@{$$self{work}}, { handler => $handler, args => \@args });
}

sub play {
    my ($self, $client) = @_;
    my $pl_entries = scalar @{$$client{playlist}};
    return $self->info("play failed because playlist is empty.\n")
        unless $pl_entries;
    $$client{pl_pos} = $pl_entries - 1
        if($$client{pl_pos} >= $pl_entries);
    if($$client{state} eq 'paused') {
        $self->send_strm($client, command    => 'u');
    } else {
        $self->stream_new_track($client, $$client{playlist}[$$client{pl_pos}]);
    }
}

sub skip_next {
    my ($self, $client) = @_;
    my $pl_entries = scalar @{$$client{playlist}};
    $self->play($client) if(++$$client{pl_pos} < $pl_entries);
}

sub skip_prev {
    my ($self, $client) = @_;
    my $pl_entries = scalar @{$$client{playlist}};
    $self->play($client) if(--$$client{pl_pos} >= 0);
}

sub pause {
    my ($self, $client) = @_;
    $self->send_strm($client, command    => 'p');
}

sub stream_new_track {
    my ($self, $client, $path) = @_;
    my $realpath = config('musicdir', '') . $path;
    if(!-f $realpath) {
        $self->info("stream_new_track: could not find $path!\n");
        return -1;
    }
    my $type;
    if($path =~ /\.([^\/]+)$/) {
        my $ext = lc($1);
        my %type_by_extension = (
            'flac' => 'f',
            'ogg'  => 'o',
            'mp3'  => 'm',
            'aac'  => 'a',
            'alac' => 'l',
            'wma'  => 'w',
        );
        if(!exists($type_by_extension{$ext})) {
            $self->info("stream_new_track: could not recognize extension '$ext'\n");
            return -1;
        }
        $type = $type_by_extension{$ext};
    } else {
        $self->info("stream_new_track: could not find file extension in $path\n");
        return -1;
    }
    my $http_path = config('httpdir', '') . $path;
    # close any previous connection it may have had.
    $self->send_strm($client, command    => 'f');
    # start the new stream
    $self->send_strm($client,
        command    => 's',
        format     => $type,
        autostart  => 1,
        http_query => "GET $http_path HTTP/1.0\r\n\r\n",
    );
}


=head1 UI

=head2 handle_keypress

Handle keys sent to us via an infrared remote control.

=cut

# unhandled keys:
#        power
#        add up play
#        left unknown right
#        unknown down browse
#        1 2 3 4 5 6 7 8 9
#        favorites 0 search
#        shuffle repeat sleep
#        nowplaying size brightness
our %keypress_handlers = (
    'volup'   => { handler => \&key_volume_handler, fluid => 1 },
    'voldown' => { handler => \&key_volume_handler, fluid => 1 },
    'rewind'  => { handler => \&key_seek_handler,   fluid => 1 },
    'fastfwd' => { handler => \&key_seek_handler,   fluid => 1 },
    'pause'   => { handler => \&key_pause_handler },
);
sub handle_keypress {
    my ($self, $client, $key, $ts) = @_;
    if(!exists($keypress_handlers{$key})) {
        $self->info("dropping unhandled key $key.\n");
        return -1;
    }
    my $fluid   = $keypress_handlers{$key}{fluid} // 0;
    my $handler = $keypress_handlers{$key}{handler};
    if(!$fluid) {
        # the client keeps sending us rapid keypress events until the button
        # is released.  The timestamp lets us filter out the duplicates.
        my $last = $$client{lastpress}{$key} // 0;
        $$client{lastpress}{$key} = $ts;
        if(($ts - $$client{repeatrate}) <= $last) {
            return -1;
        }
    }
    return &$handler($self, $client, $ts, $key);
}

sub key_volume_handler {
    my ($self, $client, $ts, $key) = @_;
    my $delta = ($key eq 'volup') ? 1 : -1;
    my $volume = $$client{volume} + $delta;
    if(0 <= $volume && $volume < 128) {
        $$client{volume} = $volume;
        $self->send_audg($client);
        return 0;
    } else {
        $self->info("limiting volume to $$client{volume}\n");
        return -1;
    }
}

sub key_pause_handler {
    my ($self, $client, $ts, $key) = @_;
    if($$client{state} eq 'paused') {
        return $self->play($client);
    }
    $self->info("key_pause_handler: current state is $$client{state}\n");
    return $self->pause($client);
}

sub key_seek_handler {
    my ($self, $client, $ts, $key) = @_;
    $$client{lastpress}{$key} = [gettimeofday];
    $$client{seek}{$key} = { mode => 'done' }
        unless exists $$client{seek}{$key};
    my $state = $$client{seek}{$key};
    my $mode = $$state{mode};
    if($mode eq 'done') {
        # new keypress.
        $mode = 'track';
        $$state{mode}  = $mode;
        $$state{begin} = [gettimeofday];
        $$state{unit}  = 1;
        $self->schedule_work(\&key_seek_timer, $self, $client, $key);
    } else {
        # if the button is held for more than a second, it's a seek, not a skip.
        if(tv_interval($$state{begin}) >= 1) {
            if($mode eq 'track') {
                $$state{mode} = 'seek';
            } else {
                $$state{unit}++;
            }
        }
    }
}

sub key_seek_timer {
    my ($self, $client, $key) = @_;
    return if exists $$client{dead};
    my $state = $$client{seek}{$key};
    my $lastseen = tv_interval($$client{lastpress}{$key});
    $lastseen *= 1000;
    if($lastseen > $$client{repeatrate}) {
        # the user has released the key, take action.
        if($$state{mode} eq 'seek') {
            my $unit = $$state{unit};
            $self->info("seek: $key $unit seconds. (implement me!)\n");
        } else {
            if($key eq 'rewind') {
                $self->skip_prev($client);
            } elsif($key eq 'fastfwd') {
                $self->skip_next($client);
            }
        }
        $$state{mode} = 'done';
    } else {
        # reschedule ourselves to check again later.
        $self->schedule_work(\&key_seek_timer, $self, $client, $key);
    }
}


=head1 TRANSMIT PATH

=head2 send_packet

Send a packet to the given client.

=cut

sub send_packet {
    my ($self, $client, $type, $payload) = @_;
    croak("type field is not 4 bytes") unless length($type) == 4;
    my $len = length($payload) + 4;
    $len = pack("n", $len);
    my $packet = $len . $type . $payload;
    $self->info("sending '$type'\n");
    $$client{sock}->print($packet);
    return length($packet);
}

sub send_strm {
    my ($self, $client, %args) = @_;
    my %defaults = (
        # This is mixed ASCII and binary.  This table is very explicit about
        # quoting ascii and hex-encoding binary, to try to help keep track of
        # it all.
        command       => 'q',        # 1 byte at offset 0
        autostart     => '0',        # 1 byte at offset 1
        format        => 'm',        # 1 byte at offset 2
#       pcmsamplesize => '?',        # 1 byte at offset 3
#       pcmsamplerate => '?',        # 1 byte at offset 4
#       pcmchannels   => '?',        # 1 byte at offset 5
#       pcmendian     => '?',        # 1 byte at offset 6
        threshold     => 0xff,       # 1 byte at offset 7
        spdif_enable  => 0x00,       # 1 byte at offset 8
        trans_period  => 0x00,       # 1 byte at offset 9
        trans_type    => '0',        # 1 byte at offset 10
        flags         => 0x00,       # 1 byte at offset 11
        output_thres  => 0x00,       # 1 byte at offset 12
        RESERVED      => 0x00,       # 1 byte at offset 13
        replay_gain   => 0x00000000, # 4 bytes at offset 14
        server_port   => 0x0050,     # 2 bytes at offset 18
        server_ip     => 0x00000000, # 4 bytes at offset 20
        http_query    => '', # variable size string
    );
    my %values = ( %defaults, %args );
    # PCM attr fields allow "?" which means "query it from the file header"
    foreach my $val (qw(pcmsamplesize pcmsamplerate pcmchannels pcmendian)) {
        if(exists($values{$val})) {
            $values{$val} = pack("C", $values{$val});
        } else {
            $values{$val} = '?';
        }
    }
    my $packet = pack('AAAAAAACCCACCCNnN',@values{qw(
        command autostart format pcmsamplesize pcmsamplerate pcmchannels pcmendian
        threshold spdif_enable trans_period
        trans_type
        flags output_thres RESERVED
        replay_gain
        server_port
        server_ip
    )}) . $values{http_query};
    return $self->send_packet($client, 'strm', $packet);
}

sub send_aude {
    my ($self, $client, %args) = @_;
    my %defaults = (
        spdif_enable => 1,
        dac_enable   => 1,
    );
    my %values = ( %defaults, %args );
    my $packet = pack('CC',@values{qw(spdif_enable dac_enable)});
    return $self->send_packet($client, 'aude', $packet);
}

sub send_audg {
    my ($self, $client, %args) = @_;
    my %defaults = (
        old_left  => $$client{volume},  # 0..127
        old_right => $$client{volume},  # 0..127
        dvc       => 1,
        preamp    => 0xff,
        new_left  => $$client{volume} << 9,  # 0..65535
        new_right => $$client{volume} << 9,  # 0..65535
        unknown   => 0,
    );
    my %values = ( %defaults, %args );
    my $packet = pack('NNCCNNC',@values{qw(
        old_left old_right
        dvc preamp
        new_left new_right
    )});
    $self->hexdump($packet);
    return $self->send_packet($client, 'audg', $packet);
}

sub send_visu {
    my ($self, $client, %args) = @_;
    my $which = 2;
    my %blank_defaults = (
        # This is all binary.
        which       => 0,
        count       => 0,
    );
    my %vumeter_defaults = (
        # This is all binary.
        which       => 1,
        count       => 6,
        channels    => 0,
        style       => 0,
        position    => 0x00000118,
        width       => 0x00000012,
        r_position  => 0x0000012e,
        r_width     => 0x00000012,
    );
    my %spectrum_defaults = (
        which       => 2,
        count       => 0x13,
        channels    => 0x00000000,
        bandwidth   => 0x00000000,
        preamp      => 0x00010000,
        l_position  => 0x00000000,
        l_width     => 0x000000a0,
        l_reversed  => 0x00000000,
        l_bar_width => 0x00000002,
        l_spacing   => 0x00000000,
        l_clipping  => 0x00000001,
        l_intensity => 0x00000001,
        l_cap       => 0x00000002,
        r_position  => 0x000000a0,
        r_width     => 0x000000a0,
        r_reversed  => 0x00000001,
        r_bar_width => 0x00000002,
        r_spacing   => 0x00000000,
        r_clipping  => 0x00000001,
        r_intensity => 0x00000001,
        r_cap       => 0x00000002,
    );
    $which = $args{which} if exists $args{which};
    my ($packval, @values);
    if($which == 0) {
        my %values = ( %blank_defaults, %args );
        $packval = "CC";
        @values = ();
    } elsif($which == 1) {
        my %values = ( %vumeter_defaults, %args );
        my $count = $values{count};
        $packval = "CCN$count";
        @values = @values{qw(
            which count
            channels style position width r_position r_width
        )};
    } elsif($which == 2) {
        my %values = ( %spectrum_defaults, %args );
        my $count = $values{count};
        $packval = "CCN$count";
        @values = @values{qw(
            which count
            channels bandwidth preamp
            l_position l_width l_reversed l_bar_width l_spacing l_clipping l_intensity l_cap
            r_position r_width r_reversed r_bar_width r_spacing r_clipping r_intensity r_cap
        )};
    } else {
        $self->info("send_visu: I don't know how to handle mode $which.\n");
        return -1;
    }
    my $packet = pack($packval, @values);
    $self->info("sending visu\n");
    return $self->send_packet($client, 'visu', $packet);
}

sub send_version {
    my ($self, $client, $version) = @_;
    return $self->send_packet($client, 'vers', $version);
}


=head1 RECEIVE PATH

=head2 handle_listener_read

The listener socket got a new connection, set up the new client.

=cut

sub handle_listener_read {
    my ($self, $listener) = @_;
    my $socket = $listener->accept();
    if(!defined($socket)) {
        $self->info("Listener socket couldn't accept new connection.\n");
        return;
    }
    my $fd   = $socket->fileno();
    my $addr = $socket->peerhost();
    $self->info("Received client connection from $addr\n");
    $$self{clients}{$fd} = {
        sock       => $socket,
        connected  => time(),
        peername   => $addr,
        inbuf      => '',
        fd         => $fd,
        state      => 'pending',
    };
    $$self{sel}->add($socket);
}


=head2 handle_client_read

The client socket sent us some data, handle it.

=cut

sub handle_client_read {
    my ($self, $socket) = @_;
    my $fd     = $socket->fileno();
    my $client = $$self{clients}{$fd};
    # append to current inbuf
    my $rv     = $socket->sysread($$client{inbuf}, 4096, length($$client{inbuf}));
    if($rv < 0) {
        $self->info("Client read returned $rv ($!)\n");
        return;
    } elsif(!$rv) {
        $self->info("Got EOF from client " . $$client{peername} . "\n");
        $$client{dead} = 1; # for timer callbacks
        $$self{sel}->remove($socket);
        delete($$self{clients}{$fd});
        return;
    }
    while($self->handle_client_command($client) > 0) {}
}


=head2 handle_client_command

The client socket sent us some data.  Check whether a command has been fully
received, and if so, dispatch to the right command handler.

Returns the number of bytes consumed.

=cut

our %recv_commands = (
    'HELO' => { parser => 'custom', handler => \&handle_HELO },
    'STAT' => { parser => 'custom', handler => \&handle_STAT },
    'RESP' => { parser => 'A*'    , handler => \&handle_RESP },
    'IR'   => { parser => 'NCCnCC', handler => \&handle_IR },
    'DSCO' => { parser => 'C'     , handler => \&handle_DSCO },
);
sub handle_client_command {
    my ($self, $client) = @_;
    my $addr = $$client{peername};
    my $buf  = $$client{inbuf};
    my $blen = length($buf);
    return 0 if $blen < 8;
    my ($cmd, $datalen) = unpack("A4N", $buf);
    return 0 if $blen < (8 + $datalen);
    # the full packet has arrived, remove it from the inbuf.
    $buf = substr($$client{inbuf}, 0, $datalen + 8, '');
    $buf = substr($buf, 8); # ... and strip off the command/size prefix
    if(!exists($recv_commands{$cmd})) {
        $self->info("$addr: I don't know how to handle command $cmd (len $datalen).\n");
        return -1;
    }
    my ($parser, $handler) = @{$recv_commands{$cmd}}{qw(parser handler)};
    if($parser eq 'custom') {
        &$handler($self, $client, $buf);
    } else {
        my @args = unpack($parser, $buf);
        &$handler($self, $client, @args);
    }
    return $datalen + 8;
}


=head1 NETWORK PROTOCOL HANDLERS

These are special methods which each handle a specific kind of received packet
from the client device.

=head2 handle_HELO

This is a custom handler, the amount of data varies depending on the client
type and version.

=cut

sub handle_HELO {
    my ($self, $client, $data) = @_;
    $self->info("got to handle_HELO\n");
    my %device_types = (
        2  => 'squeezebox',
        3  => 'softsqueeze',
        4  => 'squeezebox2',
        5  => 'transporter',
        6  => 'softsqueeze3',
        7  => 'receiver',
        8  => 'squeezeslave',
        9  => 'controller',
        10 => 'boom',
        11 => 'softboom',
        12 => 'squeezeplay',
    );
    my $type = vec($data, 0, 8);
    if(exists($device_types{$type})) {
        $$client{type} = $device_types{$type};
    } else {
        $$client{type} = "unknown ($type)";
    }
    $$client{revision} = vec($data, 1, 8);
    $$client{mac}      = sprintf("%02x" x 6, unpack("C6", substr($data, 2, 6)));
    my $mac            = $$client{mac};
    my $offset = 8;
    # insert more parsing here if we ever care about the remaining fields
    #$self->hexdump($data);
    # reply with our version
#    $self->send_version($client, "MiniSlim-$VERSION");

    # load per-client state.
    $$client{playlist}   = $self->perclient_config($mac, 'playlist', [@{$$self{args}}]);
    $$client{pl_pos}     = $self->perclient_config($mac, 'pl_pos',   0);
    $$client{volume}     = $self->perclient_config($mac, "volume",   10);
    $$client{repeatrate} = 200;

    $self->send_version($client, "6.5.4");
    $self->send_aude($client);
    $self->send_audg($client);
    $self->send_visu($client, which => 0);
    $self->send_visu($client, which => 2);
    $self->play($client);
    return 0;
}


=head2 handle_STAT

This is a custom handler, the amount of data varies depending on the event type.

=cut

sub handle_STAT {
    my ($self, $client, $data) = @_;
    my ($event, $crlf, $minit, $mmode, $bufsize, $buffull, $streamed, $signal,
        $jiffies, $outbufsize, $outbuffull, $ts, $error);
    if(length($data) == 43) {
        # The docs say this is variable length, but this is the only one I've seen.
        $event = '';
        my @list = unpack("A4CCCNNQnNNNNn", $data);
        ($event, $crlf, $minit, $mmode, $bufsize, $buffull, $streamed, $signal,
            $jiffies, $outbufsize, $outbuffull, $ts, $error) = @list;
        my $percent;
        if($buffull) {
            $percent = ($buffull / $bufsize);
            $percent = int($percent * 20) + 1;
        } else {
            $percent = 0;
        }
        my $output = "$event inbuf[";
        $output .= '*' x $percent;
        $output .= '.' x (20-$percent);
        $output .= "]";
        if($outbuffull) {
            $percent = ($outbuffull / $outbufsize);
            $percent = int($percent * 20) + 1;
        } else {
            $percent = 0;
        }
        $output .= " outbuf[";
        $output .= '*' x $percent;
        $output .= '.' x (20-$percent);
        $output .= "]";
        $self->info("$output\n");
    } else {
        $self->info("got to handle_STAT\n");
        $self->hexdump($data);
    }
    my %event_state_map = (
        'STMd' => 'need next track',
        'STMp' => 'paused',
        'STMr' => 'playing',
        'STMs' => 'playing',
        'STMu' => 'stopped',
    );
    if(defined($event) && exists($event_state_map{$event})) {
        my $state = $event_state_map{$event};
        if($state eq 'need next track') {
            $self->skip_next($client);
        } else {
            $$client{state} = $state;
        }
    }
    return 0;
}


=head2 handle_RESP

This is a handler for http responses.

=cut

sub handle_RESP {
    my ($self, $client, $resp) = @_;
#    $self->info("handle_RESP\n");
#    $self->hexdump($resp);
}


=head2 handle_DSCO

This is a handler for stream disconnection notifications.

=cut

sub handle_DSCO {
    my ($self, $client, $arg) = @_;
    $self->info("handle_DSCO\n");
    $self->hexdump($arg);
}


=head2 handle_IR

This is a handler for remote control keypresses.

=cut

sub handle_IR {
    my ($self, $client, $timestamp, $format, $length, $vendor, $rkey, $ck) = @_;
    if($vendor != 0x7689) {
        $self->info("IR vendor $vendor unknown, event dropped.\n");
        return -1;
    }
    if(($rkey ^ $ck) != 0xff) {
        $self->info("checksum mismatch, key $rkey dropped.\n");
        return -1;
    }
    my $key = 0;
    # IR keys are received in reverse bit order.  re-reverse them to get the
    # original keycodes.
    foreach my $bit (0..7) {
        $key |= (($rkey & (1<<$bit)) >> $bit) << (7-$bit);
    }

    # keynames in keycode order.
    my @squeezebox_remote_keys = qw(
        voldown volup power
        rewind pause fastfwd
        add up play
        left unknown right
        unknown down browse
        1 2 3 4 5 6 7 8 9
        favorites 0 search
        shuffle repeat sleep
        nowplaying size brightness
    );
    if(!exists($squeezebox_remote_keys[$key])) {
        $self->info("Unknown key pressed, key $key dropped.\n");
        return -1;
    }
    return $self->handle_keypress($client, $squeezebox_remote_keys[$key], $timestamp);
}


=head1 AUTHOR

"Mark Glines", C<< <"mark-cpan at glines.org"> >>


=head1 BUGS

Please report any bugs or feature requests to C<bug-mini-slim at rt.cpan.org>,
or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Mini-Slim>.  I will be
notified, and then you'll automatically be notified of progress on your bug
as I make changes.


=head1 SUPPORT

You're probably already reading the "perldoc Mini::Slim" output, so I won't
bother pointing you there.

You might find additional information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Mini-Slim>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Mini-Slim>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Mini-Slim>

=item * Search CPAN

L<http://search.cpan.org/dist/Mini-Slim/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT & LICENSE

Copyright 2009 "Mark Glines", all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut

1;
