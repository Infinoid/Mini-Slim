package Mini::Slim;

use warnings;
use strict;
use 5.006;
use v5.10.0;

use File::Basename;
use GD;
use IO::Select;
use IO::Socket::INET;
use Time::HiRes qw(tv_interval gettimeofday);
use blib;
use Mini::Slim::Util;
use base 'Mini::Slim::Util';

# attempt to use these if available, but we can live without them
our $have_audio_flac_header = 0;
eval { require Audio::FLAC::Header; $have_audio_flac_header = 1 };
our $have_mp3_info = 0;
eval { require MP3::Info; $have_mp3_info = 1 };

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


=head1 CONSTRUCTORS

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

=head2 new_client

The listener socket got a new connection, set up the new client.

=cut

sub new_client {
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
        sock      => $socket,
        connected => time(),
        addr      => $addr,
        inbuf     => '',
        fd        => $fd,
        state     => 'pending',
    };
    $$self{sel}->add($socket);
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
                $self->new_client($handle);
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

sub stop {
    my ($self, $client) = @_;
    $self->send_strm($client, command => 'q');
    $self->send_strm($client, command => 'f');
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
    $self->send_strm($client, command => 'p');
}

sub stream_new_track {
    my ($self, $client, $path) = @_;
    delete($$client{tracklen});
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
    $self->schedule_work(\&find_track_len, $self, $client, $realpath);
}


=head1 UI Input

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
    'volup'      => { handler => \&key_volume_handler, fluid => 1 },
    'voldown'    => { handler => \&key_volume_handler, fluid => 1 },
    'rewind'     => { handler => \&key_seek_handler,   fluid => 1 },
    'fastfwd'    => { handler => \&key_seek_handler,   fluid => 1 },
    'pause'      => { handler => \&key_pause_handler },
    'brightness' => { handler => \&key_brightness_handler },
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
    $self->schedule_work(\&key_volume_timer, $self, $client)
        unless exists($$client{volumedialog});
    $$client{volumedialog} = 20;
    if(0 <= $volume && $volume < 128) {
        $$client{volume} = $volume;
        $self->send_audg($client);
        $self->update_volume_display($client);
        $self->update_display($client);
        return 0;
    } else {
        $self->info("limiting volume to $$client{volume}\n");
        return -1;
    }
}

sub key_volume_timer {
    my ($self, $client) = @_;
    return if exists $$client{dead};
    if($$client{volumedialog} > 0) {
        $$client{display} = $$client{displays}{volume};
        $$client{volumedialog}--;
        $self->schedule_work(\&key_volume_timer, $self, $client);
    } else {
        delete($$client{volumedialog});
        # FIXME: detect whether we should return to another overlay
        $$client{display} = $$client{displays}{playback};
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
        # if the button is held for more than a half second, it's a seek, not a skip.
        if(tv_interval($$state{begin}) >= 0.5) {
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
            $self->update_trackchange_display($client);
            $self->update_display($client);
            if(!exists($$client{trackchange})) {
                $self->schedule_work(\&track_change_timer, $self, $client);
            }
            $$client{trackchange} = 20;
        }
        $$state{mode} = 'done';
    } else {
        # reschedule ourselves to check again later.
        $self->schedule_work(\&key_seek_timer, $self, $client, $key);
    }
}

sub track_change_timer {
    my ($self, $client) = @_;
    return if exists $$client{dead};
    if($$client{trackchange} > 0) {
        $$client{display} = $$client{displays}{trackchange};
        $$client{trackchange}--;
        $self->schedule_work(\&track_change_timer, $self, $client);
    } else {
        delete($$client{trackchange});
        # FIXME: detect whether we should return to another overlay
        $$client{display} = $$client{displays}{playback};
    }
}

sub key_brightness_handler {
    my ($self, $client, $ts, $key) = @_;
    return if $$client{displaytype} eq 'unknown';
    $$client{brightness}++;
    $$client{brightness} = 0 if $$client{brightness} > 4;
    $self->send_grfb($client, level => $$client{brightness});
}


=head1 UI Output

=head2 setup_display

Set up the displays for this client.

=cut

sub setup_display {
    my ($self, $client) = @_;
    $self->send_grfb($client, level => $$client{brightness});
    $self->send_visu($client, which => 0);
    $self->send_visu($client, which => 2);
    my ($xsize, $ysize) = ($$client{displaytype} =~ /(\d+)x(\d+)/);
    $$client{xsize} = $xsize;
    $$client{ysize} = $ysize;
    $self->info("setup_display: $xsize x $ysize\n");
    $$client{displays} = {};
    for my $displayname ('playback', 'volume', 'trackchange') {
        my $display = $$client{displays}{$displayname} = {};
        $$display{gd} = GD::Image->new($xsize, $ysize);
        $$display{gd_white} = $$display{gd}->colorAllocate(255,255,255);
        $$display{gd_black} = $$display{gd}->colorAllocate(0,0,0);
        $$display{gd}->filledRectangle(0, 0, $xsize-1, $ysize-1, $$display{gd_black});
    }
    $$client{display} = $$client{displays}{playback};
}

=head2 update_display

Shovel bits from the GD buffer to the client display.

Currently this uses WBMP as an intermediate format, but I'm open to better
ideas if anyone has one.

=cut

sub update_display {
    my ($self, $client) = @_;
    my $xsize     = $$client{xsize};
    my $ysize     = $$client{ysize};
    my $xstart    = 0;
    my $colshiftB = ${ {
        32 => 2, # 32 bits = 4 bytes = 1<<2
    }}{$ysize};
    my $colshiftb = $colshiftB + 3;
    my $outbuf;
    my $wbmp      = $$client{display}{gd}->wbmp($$client{display}{gd_black});
    # strip off header
    substr($wbmp, 0, 5, '');
    my @wbmp_yboffsets = map {
        ($xsize & 7 ? ($xsize+8) & 0xfffffff8 : $xsize) * $_
    } (0..$ysize-1);
    my @wbmp_xboffsets = map {
        ($_+7) - (($_ & 7) << 1)
    } (0..$xsize-1);
    my @wbmp_yBoffsets = map { $_ >> 3 } (@wbmp_yboffsets);
    my $x = $xstart;
    while($x < $xsize) {
        my $outbuf_column_offset = ($x-$xstart) << $colshiftb;
#        if((!($x & 0x7)) && (($xend - $x) >= 8)) {
#            # handle whole bytes at a time.
#            foreach my $y (0..$ysize-1) {
#                my $yoff = $outbuf_column_offset + $y;
#                my $byte = vec($wbmp, $wbmp_yBoffsets[$y] + $x, 8);
#                foreach my $bit (0..7) {
#                    vec($outbuf, $yoff + ($bit << $colshiftb), 1) = ($byte & 1);
#                    $byte >>= 1;
#                }
#            }
#            $x += 8;
#        } else
        {
            # handle a single column.
            my $bitstring = '';
            my $xboffset = $wbmp_xboffsets[$x];
            foreach my $y (0..$ysize-1) {
                $bitstring .= vec($wbmp, $xboffset + $wbmp_yboffsets[$y], 1);
            }
            $outbuf .= pack("B$ysize", $bitstring);
            $x++;
        }
    }
    $self->send_grfe($client,
        offset     => 0,
        transition => 'c',
        param      => 0,
        data       => $outbuf
    );
}

=head2 render_text_at

Render text on the client's display.

=cut

sub render_text_at {
    my ($self, $display, $x, $y, $xend, $yend, $text, $font) = @_;
    $font = GD::Font->Small() unless defined $font;
    $$display{gd}->filledRectangle($x, $y, $xend, $yend, $$display{gd_black});
    $$display{gd}->string($font, $x+1, $y, $text, $$display{gd_white});
}


=head2 render_bar_at

Render a progress bar on the client's display.  The "value" should be a
decimal in the range 0 to 1, inclusive.

=cut

sub render_bar_at {
    my ($self, $display, $x, $y, $xend, $yend, $value) = @_;
    $value = 0 if $value < 0;
    $value = 1 if $value > 1;
    my $diameter = $yend - $y;
    my $radius = ($diameter) / 2;
    my $full = int($value * ($xend-$x));
    # clear old data
    $$display{gd}->filledRectangle($x, $y, $xend, $yend, $$display{gd_black});
    # white rectangle
    $$display{gd}->rectangle($x, $y, $xend, $yend, $$display{gd_white});
    # vertical line to bound the filled region
    $$display{gd}->line($x+$full, $y, $x+$full, $yend, $$display{gd_white});
    # fill the left part with white
    if($full > 0) {
        $$display{gd}->fill($x+$full-1, $y+$radius, $$display{gd_white});
    }
    # round off the left end
    $$display{gd}->arc($x   +$radius, $y+$radius, $diameter  , $diameter  , 90 , 270, $$display{gd_white});
    $$display{gd}->arc($x   +$radius, $y+$radius, $diameter+1, $diameter+1, 90 , 270, $$display{gd_black});
    $$display{gd}->fill($x, $y   , $$display{gd_black});
    $$display{gd}->fill($x, $yend, $$display{gd_black});
    # round off the right end
    $$display{gd}->arc($xend-$radius, $y+$radius, $diameter  , $diameter  , 270, 90 , $$display{gd_white});
    $$display{gd}->arc($xend-$radius, $y+$radius, $diameter+1, $diameter+1, 270, 90 , $$display{gd_black});
    $$display{gd}->fill($xend, $y   , $$display{gd_black});
    $$display{gd}->fill($xend, $yend, $$display{gd_black});
}

=head2 update_track_pos

Updates the track position and playback progress bar on the playback display.

=cut

sub update_track_pos {
    my ($self, $client) = @_;
    my $playback = $$client{displays}{playback};
    my $seconds = $$client{trackpos};
    my $seconds_part = $seconds % 60;
    my $minutes_part = ($seconds - $seconds_part) / 60;
    $seconds_part = "0$seconds_part" while length($seconds_part) < 2;
    $self->render_text_at($playback, 0, 0, 239, 12, "$minutes_part:$seconds_part");
    if(defined($$client{tracklen})) {
        my $tracklen = int($$client{tracklen});
        $seconds_part = $tracklen % 60;
        $minutes_part = ($tracklen - $seconds_part) / 60;
        $seconds_part = "0$seconds_part" while length($seconds_part) < 2;
        $self->render_text_at($playback, 190, 0, 239, 12, "$minutes_part:$seconds_part");
        my $full = $seconds / $$client{tracklen};
        $self->render_bar_at($playback, 44, 3, 180, 11, $full);
    }
}

=head2 update_track_name

Updates the track name on the playback display.

=cut

sub update_track_name {
    my ($self, $client) = @_;
    my $playback = $$client{displays}{playback};
    return unless $$client{state} eq 'playing';
    my $font = GD::Font->Large();
    my $track = $$client{playlist}[$$client{pl_pos}];
    $self->render_text_at($playback, 0, 15, 320, 31, basename($track), $font);
}

=head2 update_playlist_pos

Updates the current track position on the playback display.

=cut

sub update_playlist_pos {
    my ($self, $client) = @_;
    my $playback = $$client{displays}{playback};
    my $pos = $$client{pl_pos} + 1;
    my $total = scalar @{$$client{playlist}};
    $self->render_text_at($playback, 240, 0, 320, 12, "track $pos/$total");
}


=head2 update_volume_display

Re-draw the volume display.

=cut

sub update_volume_display {
    my ($self, $client) = @_;
    my $display = $$client{displays}{volume};
    my $volume = $$client{volume};
    my $minvol = 0;
    my $maxvol = 127;
    $self->render_text_at($display, 0, 0, 239, 12, "Volume: $volume/$maxvol");
    my $full = $volume / $maxvol;
    $self->render_bar_at($display, 20, 15, $$client{xsize}-20, $$client{ysize}-3, $full);
}


=head2 update_trackchange_display

Re-draw the trackchange display.

=cut

sub update_trackchange_display {
    my ($self, $client) = @_;
    my $display = $$client{displays}{trackchange};
    my $pos = $$client{pl_pos} + 1;
    my $total = scalar @{$$client{playlist}};
    my $fn = basename($$client{playlist}[$$client{pl_pos}]);
    $self->render_text_at($display, 20, 3 , $$client{xsize}, 14, "Skipping to track $pos (of $total)");
    $self->render_text_at($display, 20, 15, $$client{xsize}, $$client{ysize}, $fn);
}


=head1 RECEIVE PATH

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
        $self->info("Got EOF from client " . $$client{addr} . "\n");
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
    my $addr = $$client{addr};
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
    my %device_types = (
        1  => 'slimp3', # not in the documentation, but this is my guess
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
    my %display_types = (
        squeezebox2   => '320x32',
        softsqueeze3  => '320x32',
    );
    my $type = vec($data, 0, 8);
    if(exists($device_types{$type})) {
        $$client{type} = $device_types{$type};
    } else {
        $$client{type} = "unknown ($type)";
    }
    if(exists($display_types{$$client{type}})) {
        $$client{displaytype} = $display_types{$$client{type}};
    } else {
        $self->info("I don't know what kind of display client type $$client{type} has.\n");
        $$client{displaytype} = 'unknown';
    }
    $$client{revision} = vec($data, 1, 8);
    $self->info("$$client{addr} is $$client{type} version $$client{revision}.\n");
    $$client{mac}      = sprintf("%02x" x 6, unpack("C6", substr($data, 2, 6)));
    my $mac            = $$client{mac};
    my $offset = 8;
    # insert more parsing here if we ever care about the remaining fields
    #$self->hexdump($data);
    # reply with our version
    $self->send_version($client, "MiniSlim-$VERSION");

    # load per-client state.
    $$client{playlist}   = $self->perclient_config($mac, 'playlist'  , [@{$$self{args}}]);
    $$client{pl_pos}     = $self->perclient_config($mac, 'pl_pos'    , 0);
    $$client{volume}     = $self->perclient_config($mac, 'volume'    , 10);
    $$client{brightness} = $self->perclient_config($mac, 'brightness', 3);
    $$client{repeatrate} = 200;

#    $self->send_version($client, "6.5.4");
    $self->send_aude($client);
    $self->send_audg($client);
    $self->schedule_work(\&setup_display, $self, $client)
        if($$client{displaytype} ne 'unknown');
    $self->stop($client);
    $self->schedule_work(\&play, $self, $client);
    return 0;
}


=head2 handle_STAT

This is a custom handler, the amount of data varies depending on the event type.

=cut

sub handle_STAT {
    my ($self, $client, $data) = @_;
    my ($event, $crlf, $minit, $mmode, $bufsize, $buffull, $streamedH, $streamedL,
        $signal, $jiffies, $outbufsize, $outbuffull, $tracksec, $voltage, $trackmsec,
        $serverts, $error, $parsed);
    $parsed = 0;
    my $datalen = length($data);
    if($datalen == 43) {
        # Some older squeezebox2 firmware format.
        $event = '';
        my @list = unpack("A4CCCN4nNNNNn", $data);
        ($event, $crlf, $minit, $mmode, $bufsize, $buffull, $streamedH, $streamedL,
         $signal, $jiffies, $outbufsize, $outbuffull, $tracksec, $error) = @list;
        $parsed = 1;
    } elsif($datalen == 53) {
        # squeezebox2 version 130.
        $event = '';
        my @list = unpack("A4CCCN4nNNNNnNNn", $data);
        ($event, $crlf, $minit, $mmode, $bufsize, $buffull, $streamedH, $streamedL,
         $signal, $jiffies, $outbufsize, $outbuffull, $tracksec, $voltage, $trackmsec,
            $serverts, $error) = @list;
        $parsed = 1;
    } elsif($datalen == 51) {
        # This is the format softsqueeze always seems to send.
        $event = '';
        my @list = unpack("A4CCCN4nNNNNnNN", $data);
        ($event, $crlf, $minit, $mmode, $bufsize, $buffull, $streamedH, $streamedL,
         $signal, $jiffies, $outbufsize, $outbuffull, $tracksec, $voltage, $trackmsec,
            $serverts) = @list;
        $parsed = 1;
    } else {
        $self->info("got to handle_STAT: length = $datalen\n");
        $self->hexdump($data);
    }
    if($parsed) {
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
#        $self->info("$output\n");
        my %event_state_map = (
            'STMd' => 'need next track',
            'STMp' => 'paused',
            'STMr' => 'playing',
            'STMs' => 'playing',
            'STMu' => 'stopped',
        );
        if(exists($event_state_map{$event})) {
            my $state = $event_state_map{$event};
            if($state eq 'need next track') {
                $self->skip_next($client);
            } else {
                $$client{state} = $state;
                if($state eq 'playing') {
                    if($$client{displaytype} ne 'unknown') {
                        $self->schedule_work(\&update_track_name, $self, $client);
                        $self->schedule_work(\&update_playlist_pos, $self, $client);
                        $self->schedule_work(\&update_display, $self, $client);
                    }
                }
            }
        }
        if($event eq 'STMt') {
            $$client{trackpos} = $tracksec;
            if($$client{displaytype} ne 'unknown') {
                $self->schedule_work(\&update_track_pos, $self, $client);
                $self->schedule_work(\&update_display, $self, $client);
            }
        }
        return 0;
    }
    return -1;
}


=head2 handle_RESP

This is a handler for http responses.  Essentially we just ignore it for now.

=cut

sub handle_RESP {
    my ($self, $client, $resp) = @_;
}


=head2 handle_DSCO

This is a handler for stream disconnection notifications.  Essentially we just
ignore it for now.

=cut

sub handle_DSCO {
    my ($self, $client, $arg) = @_;
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
#    $self->info("sending '$type'\n");
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
    return $self->send_packet($client, 'audg', $packet);
}

sub send_grfb {
    my ($self, $client, %args) = @_;
    my %defaults = (
        level => 3,
    );
    my %values = ( %defaults, %args );
    my $packet = pack('n',@values{qw(level)});
    return $self->send_packet($client, 'grfb', $packet);
}

sub send_grfe {
    my ($self, $client, %args) = @_;
    my %defaults = (
        offset     => 0,
        transition => 'c',
        param      => 0,
        data       => '',
    );
    my %values = ( %defaults, %args );
    my $packet = pack('nAC',@values{qw(offset transition param)}) . $values{data};
    return $self->send_packet($client, 'grfe', $packet);
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
    return $self->send_packet($client, 'visu', $packet);
}

sub send_version {
    my ($self, $client, $version) = @_;
    return $self->send_packet($client, 'vers', $version);
}


=head1 MISC GUTS

=head2 find_track_len

Given a filename, scan the file to try to find its length (in seconds).

=cut

sub find_track_len {
    my ($self, $client, $fn) = @_;
    my ($ext) = ($fn =~ /\.([^\/]+)$/);
    return unless defined $ext;
    $ext = lc($ext);
    if($ext eq 'flac') {
        return unless $have_audio_flac_header;
        my $header = Audio::FLAC::Header->new($fn);
        return unless defined $header;
        my $seconds = $header->info->{TOTALSAMPLES} / $header->info->{SAMPLERATE};
        $$client{tracklen} = $seconds;
    }
    if($ext eq 'mp3') {
        return unless $have_mp3_info;
        my $header = MP3::Info->new($fn);
        return unless defined $header;
        $$client{tracklen} = $header->{SECS};
    }
}


=head1 AUTHOR

"Mark Glines", C<< <"mark-cpan at glines.org"> >>


=head1 BUGS

It works for me.  I have a device that (currently) reports itself as a
squeezebox2 version 130.  There's no guarantee it will work for your device, or
your firmware version.  That said, I'll happily consider applying patches that
make it work with other models and versions.  Especially if those patches also
contain test cases for the things your player does differently.

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

=item * Github: The source repository

L<http://github.com/Infinoid/Mini-Slim>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Mini-Slim>

=back


=head1 ACKNOWLEDGEMENTS

Mad props to slim devices for making nifty hardware.

Also, props to whoever wrote this:

    http://wiki.slimdevices.com/index.php/SlimProtoTCPProtocol

It isn't always accurate, but it's a whole lot better than nothing.


=head1 COPYRIGHT & LICENSE

Copyright 2009, 2014 "Mark Glines", all rights reserved.

It is distributed under the terms of the Artistic License 2.0.
For more details, see the full text of the license in the file LICENSE.


=cut

1;
