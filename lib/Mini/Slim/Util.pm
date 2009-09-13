package Mini::Slim::Util;

use warnings;
use strict;

use Perl6::Export::Attrs;
use Carp;
use YAML qw(LoadFile);

=head1 NAME

Mini::Slim::Util - Secondary utility methods and functions for Mini::Slim


=head1 METHODS

=head2 info

Log an informational message.

=cut

sub info {
    my ($self, @message) = @_;
    print("info: ", @message);
}

=head2 hexdump

    $self->hexdump($buffer);

Hexdump the given buffer using $self->info().

=cut

sub hexdump {
    my ($self, $buffer) = @_;
    my $len = length($buffer);
    my $output = '';
    foreach my $i (0..$len-1) {
        if(!($i & 15)) {
            if($i) {
                $output .= "  ";
                my $substr = substr($buffer, $i - 16, 16);
                $substr =~ s/[^\d\w`~!@#$%^&*()\-_=+\[\]{}\\|;:'",<>\.\/? ]/./g;
                $self->info($output . $substr . "\n");
            }
            $output = sprintf("%04x", $i);
        }
        $output .= ' ' if !($i & 7);
        $output .= sprintf(" %02x", vec($buffer, $i, 8));
    }
    $self->info($output . "\n") if length $output;
}


=head1 FUNCTIONS

=head2 first_line

    ($line, $buf) = first_line($buf);

Given a string buffer with zero or more newlines, split out the first full line
of text, and return that, and the remainder.  Returns an array of two scalars:
The line, and the remaining data.

The line is returned without the trailing newline.  If no full line exists,
the first return value will be undef.

The remainder will consist of everything after the first newline.  The newline
itself will be dropped.  If no more text is available, the second return value
will be an empty string (like '').

=cut

sub first_line :Export(:DEFAULT) {
    my $buf = shift;
    croak "undefined argument" unless defined $buf;
    my $i = index($buf,"\n");
    my $rv = undef;
    if($i >= 0) {
        $rv = substr($buf,0,$i);
        $buf = substr($buf,$i+1);
    }
    return ($rv, $buf);
}


=head2 config

    my $value = config("variable_name", $default);

Looks up a config value in the config files.  If the value cannot be found,
the specified default is returned, instead.

The following config files are read, in order:

* A file pointed to by the MINISLIM environment variable
* ".mini-slim" in the user's HOME directory
* /etc/mini-slim.conf

The content of these files are read at module-load time.

=cut

my $loaded_config_before = 0;
our @config; # marked as "our" so the testsuite can override it
if(!$loaded_config_before) {
    foreach my $config_file (
            $ENV{MINISLIM},
            $ENV{HOME}."/.mini-slim",
            "/etc/mini-slim.conf") {
        next unless defined($config_file);
        next unless -r $config_file;
        push(@config, LoadFile($config_file));
    }
}
sub config :Export(:DEFAULT) {
    my ($keyword, $value) = @_;
    my $wantarray = wantarray();
    foreach my $config (@config) {
        if(exists $$config{$keyword}) {
            $value = $$config{$keyword};
            last;
        }
    }
    if($wantarray) {
        return @$value if ref($value) eq 'ARRAY';
        return () if scalar @_ == 1;
        return () unless defined $value;
    }
    return $value;
}


=head2 perclient_config

    my $value = $self->perclient_config($mac, "variable_name", $default);

Fetch per-client state settings, falling back on global config (and eventually,
the provided default) if needed.

=cut

my %perclient_state;
sub perclient_config {
    my ($self, $mac, $keyword, $default) = @_;
    $self->info("perclient_config: mac=$mac keyword=$keyword\n");
    return config($keyword, $default);
#    if(!exists
#        push(@config, LoadFile($config_file));
}

=head1 AUTHOR

"Mark Glines", C<< <"mark-cpan at glines.org"> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-mini-slim at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Mini-Slim>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You're probably already reading the "perldoc Mini::Slim" output, so I won't
bother pointing you there.

You may find additional information at:

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
