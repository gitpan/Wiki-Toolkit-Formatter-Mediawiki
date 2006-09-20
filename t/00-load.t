#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Wiki::Toolkit::Formatter::Mediawiki' );
}

diag( "Testing Wiki::Toolkit::Formatter::Mediawiki $Wiki::Toolkit::Formatter::Mediawiki::VERSION, Perl $], $^X" );
