
require 5.008_001;
use ExtUtils::MakeMaker;

WriteMakefile( 
	'DISTNAME'	=> 'S-SymObj',
	'NAME'		=> 'SymObj',
	'VERSION_FROM'	=> 'lib/SymObj.pm',
	(
	ABSTRACT	=> 'Easy creation of symbol-tables and objects',
	AUTHOR		=> 'Steffen "Daode" Nurpmeso <sdaoden@users.sf.net>',
	LICENSE		=> 'ISC',
	)
);

# vim:set fenc=utf-8 syntax=perl ts=8 sts=8 sw=8 noet tw=79:
