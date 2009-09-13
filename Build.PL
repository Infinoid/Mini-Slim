use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Mini::Slim',
    license             => 'perl',
    dist_author         => '"Mark Glines" <"mark-cpan@glines.org">',
    dist_version_from   => 'lib/Mini/Slim.pm',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Mini-Slim-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
