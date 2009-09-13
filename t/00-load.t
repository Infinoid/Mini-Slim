#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Mini::Slim' );
}

diag( "Testing Mini::Slim $Mini::Slim::VERSION, Perl $], $^X" );
