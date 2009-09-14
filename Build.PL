use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Mini::Slim',
    license             => 'perl',
    dist_author         => '"Mark Glines" <"mark-cpan@glines.org">',
    dist_version_from   => 'lib/Mini/Slim.pm',
    build_requires => {
        'perl'                 => '5.10.0', # which includes the following...
#        'Carp'                 => 0,
#        'File::Basename'       => 0,
#        'IO::Select'           => 0,
#        'IO::Socket::INET'     => 0,
#        'Test::More'           => 0,
#        'Time::HiRes'          => 0,

        'GD'                   => 0,
        'Perl6::Export::Attrs' => 0,
        'YAML'                 => 0,
    },
    add_to_cleanup      => [ 'Mini-Slim-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
