# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# Time-stamp: "1999-03-06 15:59:05 MST"
######################### We start with some black magic to print on failure.

# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1\n"; }
END {print "BAD! 1\n" unless $loaded;}
use Locale::Maketext;
$loaded = 1;
print "OK 1\n";

######################### End of black magic.
