# This -*- perl -*- script writes the Makefile for Locale::Maketext
# Time-stamp: "2003-04-02 11:08:28 AHST"
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
   NAME          => 'Locale-Maketext',
   VERSION_FROM  => 'lib/Locale/Maketext.pm',
   ABSTRACT_FROM => 'lib/Locale/Maketext.pod',
   'PREREQ_PM'    => {	
	                 'I18N::LangTags' => 0.21,
                         $^O =~ m/Win32/i ? (
			    'Win32::Locale' => 0.01,
					   ) : (),
	             },
   'dist'        => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);

package MY;

sub libscan
{ # Determine things that should *not* be installed
    my($self, $path) = @_;
    return '' if $path =~ m/~/;
    $path;
}

__END__
