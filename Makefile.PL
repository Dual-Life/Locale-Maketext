# This -*- perl -*- script writes the Makefile for Locale::Maketext
# Time-stamp: "1999-03-15 22:14:53 MST"
require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
   NAME          => 'Locale-Maketext',
   VERSION_FROM  => 'lib/Locale/Maketext.pm',
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
