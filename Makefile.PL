# This -*- perl -*- script writes the Makefile for Locale::Maketext
# Time-stamp: "2004-03-30 16:32:21 AST"
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
   NAME          => 'Locale-Maketext',
   VERSION_FROM  => 'lib/Locale/Maketext.pm',
   ABSTRACT_FROM => 'lib/Locale/Maketext.pod',
   'PREREQ_PM'    => {	
	                 'I18N::LangTags' => 0.30,
                        # $^O =~ m/Win32/i ? (
			#    'Win32::Locale' => 0.01,
			#		   ) : (),
	             },
   'dist'        => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
   ($] < 5.008) ? () : ( INSTALLDIRS => 'perl' ),
     # If under a version with Maketext in core, overwrite that core file.
);
