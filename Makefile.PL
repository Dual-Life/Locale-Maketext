# This -*- perl -*- script writes the Makefile for Locale::Maketext
# Time-stamp: "2001-05-25 08:17:30 MDT"
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
   NAME          => 'Locale-Maketext',
   VERSION_FROM  => 'lib/Locale/Maketext.pm',
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
