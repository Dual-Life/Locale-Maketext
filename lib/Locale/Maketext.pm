#!/bin/false
# Time-stamp: "1999-03-15 22:04:29 MST"

require 5;
package Locale::Maketext;
use strict;
use vars qw( @ISA $Debug $VERSION $MATCH_SUPERS $USING_LANGUAGE_TAGS
             $SELECTED);
use Carp;
use I18N::LangTags 0.12;

$Debug = 0 unless defined $Debug;
$VERSION = "0.17";
@ISA = ();

$MATCH_SUPERS = 1;
$USING_LANGUAGE_TAGS = 1;
 # Turning this off is somewhat of a security risk in that little or no
 # checking will be done on the legality of tokens passed to the
 # eval("use $module_name") in _try_use.  If you turn this off, you have
 # to do your own checking.

###########################################################################

=head1 NAME

Locale::Maketext -- framework for software localization

=head1 SYNOPSIS

  [to be written]

=head1 DESCRIPTION

Locale::Maketext is a base class providing a framework for
inheritance-based lexicons for software localization.

Further documentation is forthcoming.

=cut

###########################################################################
# Sane defaults

=head1 Methods You May Override

These are all simple methods you should feel welcome to override as
you like.

=over

=item the method $LH->encoding

[to be written]

=cut

sub encoding {
  my $it = $_[0];
  return(
   (ref($it) && $it->{'encoding'})
   || "iso-8859-1"   # Latin-1
  );
} 

#--------------------------------------------------------------------------

=item the method $LH->fallback_languages

[to be written]

=cut

sub fallback_languages { return('i-default', 'en', 'en-US') }

#--------------------------------------------------------------------------

=item the method $LH->sprintf(FORMAT, PARAMS)

Just a wrapper for the normal Perl builtin function sprintf -- it's here
so you can use sprintf in bracketish notation.

=cut

sub sprintf {
  my($handle, $format, @params) = @_;
  return sprintf($format, @params);
}

=item the method $LH->quant

[to be written]

=cut

sub quant {
  my($handle, $num, @forms) = @_;

  return $num if @forms == 0; # what should this mean?
  return $forms[3] if @forms == 3 and $num == 0; # special zeroth case

  # Normal case:
  # Note that the formatting of $num is preserved.
  return( $handle->numf($num) . ' ' . $handle->numerate($num, @forms) );
   # Most human languages put the number phrase before the qualified phrase.
}

=item the method $LH->numerate

[to be written]

=cut

sub numerate { # return this item in a form appropriate to this number
  my($handle, $num, @forms) = @_;
  my $s = ($num == 1);

  return '' unless @forms;
  if(@forms == 1) { # only the headword form specified
    return $s ? $forms[0] : ($forms[0] . 's'); # very cheap hack.
  } else { # sing and plural were specified
    return $s ? $forms[0] : $forms[1];
  }
}

#--------------------------------------------------------------------------

=item the method $LH->numf(NUMBER)

This returns NUMBER formatted appropriately for the conventions of
$LH's language.  The default behavior for this is just to get
sprintf("%g", NUMBER), then to tr/,./.,/ it if $LH->{'numf_comma'} is
true, and then to return it.

=cut

sub numf {
  my($handle, $num) = @_[0,1];
  $num = sprintf("%g", $num);
  $num =~ tr<.,><,.> if $handle->{'numf_comma'} || 0;
  return $num;
}

#--------------------------------------------------------------------------

=item the method CLASS->new

This is used by CLASS->get_handle to actually perform the
instantiation of each language-handle object into its class.
Currently it just creates and blesses an empty hashref, and calls
$LH->init on it.

=cut

sub new {
  my $class = ref($_[0]) || $_[0];
  my $handle = bless {}, $class;
  $handle->init;
  return $handle;
}

=item the method $LH->init

This is used by CLASS->new to initialize newly instantiated
language-handle objects.  Currently this is a no-op.

=cut

sub init { return } # noop

=back

=cut

###########################################################################

=head2 STUFF

=over

=item the method $LH->maketext(TEXT, PARAMS)

This looks up the entry for TEXT in the the lexicon for $LH's class
and its superclasses (via $LH->lookup(TEXT)), and, assuming it's a
coderef, returns the return value of &$coderef($LH, PARAMS), which is
presumably a single scalar.

If $LH->lookup(TEXT) failed (i.e., returned undef), then C<maketext>
fails according to whatever mechanism is set in $LH's "fail" attribute
-- if the 'fail' attribute is set to a coderef, maketext returns the
output of C<&{$fail_coderef}($LH, TEXT, PARAMS)>.  If $LH's "fail"
attribute is set to a contentful string (i.e., not undef or ""),
that's considered to be a method name, and maketext returns the output
of $handle->methodname(TEXT, PARAMS).  Otherwise, maketext returns
undef.

(Actually, in all the cases above where maketext passes values to
other subs, it passes I<copies> of TEXT and/or PARAMS, so that the
actual values passed to maketext can't be altered in-place.  This is
to parallel the way Perl copies @_ for method calls.)

=cut

sub maketext {
  # Remember, this can fail.  Failure is controllable many ways.
  my($handle, $phrase, @params) = @_;
  my $value = $handle->lookup($phrase);
  if(!defined($value)) {
    if(ref($handle) && defined($handle->{'fail'})
       && length($handle->{'fail'})) {
      my $fail = $handle->{'fail'};
      print "WARNING0: maketext fails looking for <$phrase>\n" if $Debug;
      if(ref($fail) eq 'CODE') { # it's a sub reference
	$value = &{$fail}($handle, $phrase, @params);
        # if it ever returns, it should return a good value
      } else {
	$value = $handle->$fail($phrase, @params);
        # if it ever returns, it should return a good value
      }
    } else {
      print "WARNING1: maketext fails looking for <$phrase>\n" if $Debug;
    }
    return $value;
  }

  return $value unless ref($value) eq 'CODE';

  my $ret;
  {
    local $SIG{'__DIE__'} = undef;
    eval { $ret = &$value($handle, @params) };
  }
  croak "Error in maketexting $phrase : $@\n" if $@;
  return $ret;
}

###########################################################################
#
# It's all guts and gore below here...
#

=item the constructor CLASS->get_handle(LANGUAGES)

Constructs a language handle based on the list of locale IDs and/or
language tags in the list LANGUAGES.

=cut

sub get_handle {  # This is a constructor and, yes, it CAN FAIL
  # Its class argument has to be the base class for the current
  # application's l10n files.
  my($base_class, @languages) = @_;
  $base_class = ref($base_class) || $base_class;

  unless(@languages) {  # Calling with no args is magical!  wooo, magic!
    if(length($ENV{'REQUEST_METHOD'})) { # I'm a CGI
      my $in = $ENV{'HTTP_ACCEPT_LANGUAGE'} || '';
      $in =~ s<\([\)]*\)><>g; # Kill parens'd things -- just a hack.
      @languages = &I18N::LangTags::extract_language_tags($in);
    } else { # Not running as a CGI: try to puzzle out from the environment
      if(length($ENV{'LANG'})) {
	@languages = split /,:/, $ENV{'LANG'};
         # LANG can be one lg as far as I know, but what the hey.
	print "Noting ENV languages ", join(',', @languages),"\n" if $Debug;
      }
    }
  }

  #------------------------------------------------------------------------
  print "Lgs1: ", map("<$_>", @languages), "\n" if $Debug > 0;

  if($USING_LANGUAGE_TAGS) {
    @languages = map &I18N::LangTags::locale2language_tag($_), @languages;
     # if it's a lg tag, fine, pass thru.
     # if it's a locale ID, try converting to a lg tag,
     # otherwise nix it.

    push @languages, map &I18N::LangTags::super_languages($_), @languages
     if $MATCH_SUPERS;

    @languages =  map { $_, &I18N::LangTags::alternate_language_tags($_) }
                      @languages;    # catch alternation

    @languages =  # last step...
      map {
	my $it = $_;  # copy
	$it =~ tr<-A-Z><_a-z>; # lc and turn - to _
	$it =~ tr<_a-z0-9><>cd;  # remove all but a-z0-9_
	$it;
      } @languages;
  }
  print "Lgs2: ", map("<$_>", @languages), "\n" if $Debug > 0;

  push @languages, $base_class->fallback_languages;
   # You are free to override fallback_languages to return empty-list

  my %seen = ();
  foreach my $module_name ( map { $base_class . "::" . $_ }  @languages )
  {
    next unless length $module_name; # sanity
    next if $seen{$module_name}++       # Already been here, and it was no-go
            || !&_try_use($module_name); # Try to use() it but can't it.
    return($module_name->new);
  }

  return undef;
}

#--------------------------------------------------------------------------

=item the method $LH->fail_with(FAILSPEC)

This sets the "fail" attribute of a given language handle.
See the docs for $LH->maketext(TEXT, PARAMS).

=item the method $LH->fail_with

This returns the value of $LH's "fail" attribute.

=cut

sub fail_with { # an actual attribute method!
  my($handle, @params) = @_;
  return unless ref($handle);
  $handle->{'fail'} = $params[0] if @params;
  return $handle->{'fail'};
}

#--------------------------------------------------------------------------

=item the method 'failure_handler_auto'

[to be written]

=cut

sub failure_handler_auto {
  # Meant to be used like:
  #  $handle->fail_with('failure_handler_auto')

  my($handle, $phrase, @params) = @_;
  $handle->{'failure_lex'} ||= {};
  my $lex = $handle->{'failure_lex'};

  $lex->{$phrase} ||= $handle->compile($phrase);
  return &{$lex->{$phrase}}($handle,@params);
}

###########################################################################

=item the method $LH->lookup(TEXT)

This looks up the entry TEXT in the lexicons for the handle $LH.  This
is used by $LH->maketext(TEXT).

If $LH->lookup(TEXT) looks across the lexicons for $LH (which are
gotten via $LH->lex_refs), this returns undef.  Otherwise:

For the first lexicon containing an entry for TEXT, if that entry is a
coderef (presumably thru being an anonymous sub, but it doesn't
matter), it's returned.

If, however, the entry for TEXT is a string, that entry is I<replaced>
with the contents of $LH->compile($that_lexicon{TEXT}), and that value
is returned.

Magic happens when C<lookup> looks in a lexicon where you have set
$Lexicon{'_AUTO'} = 1.  This C<_AUTO> flag means that a lookup in that
lexicon can't really fail -- if there is no entry for TEXT,
$LH->lookup(TEXT) will set $Lexicon{TEXT} = $LH->compile(TEXT), and
return that value.  This is generally useful only in base classes, as
a way to mitigate lookup failures.

=cut

sub lookup {
  # Look for things in the lexicons of my class and supers,
  #  compling as necessary.  Returns undef if unsuccessful.
  # To surpress compilation, just override compile with {return $_[0]}. 
  my $handle = $_[0]; # the handle
  my $class = ref($_[0]) || $_[0]; # the class /name/
  my $key = $_[1];

  croak "argument to handle->lookup(phrase) must not be undef!"
   unless defined $key;

  my @lex_refs = $handle->lex_refs;
  print " Lex refs for $class : ", map("<$_>", @lex_refs), "\n" if $Debug;
  foreach my $h_r (@lex_refs) {
    print "* Looking up \"$key\" in $h_r\n" if $Debug;
    if(exists $h_r->{$key}) {
      print "  Found \"$key\" in $h_r\n" if $Debug;
      return $h_r->{$key} if ref $h_r->{$key};
      return( $h_r->{$key} = $handle->compile($h_r->{$key}) );
    } elsif(exists($h_r->{'_AUTO'}) && $h_r->{'_AUTO'}) { # it's an auto lex
      print "  Automaking \"$key\" into $h_r\n" if $Debug;
      return( $h_r->{$key} = $handle->compile($key) );
    }
    print "  Not found in $h_r, nor automakable\n" if $Debug > 1;
    # else keep looking
  }
  print "! Lookup of \"$key\" in/under $class fails.\n" if $Debug;
  return undef;  # Fallthru
}

#==========================================================================

=item the method $LH->compile(TEXT) or CLASS->compile(TEXT)

This takes text in bracketish notation (presumably just retrieved via
a lexicon returned from $LH->lex_refs), and returns an anonymous sub
that is that text compiled into Perl.

Used by $LH->lookup(TEXT).

=cut

sub compile {  # Compile this string into an anonymous sub
  my $class = ref($_[0]) || $_[0];
  my $text = $_[1];

  return sub { return undef } unless defined $text;  # uh, whatever.

  # Cobbles together a sub that's a closure around @bits and maybe @methods

  print "About to compile $text\n" if $Debug;

  return sub { $text } unless $text =~ /\[/;
   # if there's no brackety things to compile, just be a closure

  my @bits = grep( length($_), split(/(\[[^\]]*\])/, $text, -1) );
  return sub { return '' } unless @bits;

  my @methods = (undef) x @bits;
  my @pre_out = ("sub {\n use strict;\n my \$handle = \$_[0];\n");
  if($Debug > 1) {
    push @pre_out,
     "  print \"\\\@_: \", join(',', \@_), \"\\n\";\n",
     "  print \"\\\@methods: \", join(',', \@methods), \"\\n\";\n",
     "  print \"\\\@bits: \", join(',', \@bits), \"\\n\";\n",
    ;
  }
  my @out = (" return join '',\n");
  my $real_bits_done = 0;

#!! TO DO: unquote things !!

  # Iterate over the bits of this phrase -- brackets or literals
  for(my $i = 0; $i < @bits; $i++) {
    # next unless length $bits[$i];
    if($bits[$i] =~ /^\[([^\]]*)\]$/s) { # It's brackety.
      next unless length $1;
      my @p = split(',', $1, -1);
       # print " <$bits[$i]> => ", map("<$_>", @p), "\n" if $Debug > 1;

      next unless @p; # sanity
      $bits[$i] = \@p;
      $methods[$i] = shift @p; # First parameter is the method name.

      # Iterate over the remaining bits of this brackety thing
      my(@p_calls) = ();
      for(my $p_i = 0; $p_i < @p; $p_i++) {
        # print "  looky #", $p_i, ":", $p[$p_i], "\n" if $Debug > 1;
        if($p[$p_i] =~ /^\s*_(\d+)\s*$/s) {  # It's like _2 for $_[2]
	  push @p_calls, "\$_[", (0 + $1), "], ";
        } else { # it's a literal
	  push @p_calls, "\$bits[$i][$p_i], ";
        }
      }
      next unless length($methods[$i]) && $methods[$i] =~ /\S/; # sanity

      push @pre_out, " my \$m$i = \$methods[$i];\n";
      push @out, "  \$handle->\$m$i(", @p_calls, "),\n";
      # push @out, "  # ", join('~', @{$bits[$i]} ), "\n" if $Debug;
      $real_bits_done++;

    } else { #  It's just a literal
      push @out, "  \$bits[$i],\n";
      $real_bits_done++;
    }
  }
  push @out, "  '', # no content!?\n" unless $real_bits_done; # sanity
  push @out, "}\n";

  print "Code to eval:\n", @pre_out, @out if $Debug > 1;
  my $sub = eval(join('', @pre_out, @out));
  croak "Shocking! Error $@ in compilation of $text !?!?\n" if $@;
  print "Compiled OK\n" if $Debug;
  return $sub;
}

###########################################################################
my %tried = ();
  # memoization of whether we've used this module, or found it unnecessary.
sub _try_use {   # Basically a wrapper around "use Modulename"
  # "Many men have tried..."  "They tried and failed?"  "They tried and died."
  my $module = $_[0];   # ASSUME sane module name!

  return $tried{$module} if exists $tried{$module};  # memoization

  { no strict;
    return($tried{$module} = 1)
     if defined(%{$class . "::Lexicon"}) || defined(@{$class . "::ISA"});
    # weird case: we never use'd it, but there it is!
  }

  print " About to use $module ...\n" if $Debug;
  {
    local $SIG{'__DIE__'} = undef;
    eval "use $module";
  }
  if($@) {
    print "Error using $module \: $@\n" if $Debug > 1;
    return($tried{$module} = 0);
  } else {
    print " OK, $module is used\n" if $Debug;
    return($tried{$module} = 1);
  }
}
#--------------------------------------------------------------------------

=item the method $LH->lex_refs

This returns a list of hashrefs which are the lexicons for the class
that this handle belongs to.  This is figured out by looking in the
package for $LH's class, and the packages of the classes in its @ISA
tree.  The results of this are memoized for each class.  This assumes
that no class's @ISA will change after you've started calling maketext
methods.  I think this is a sane assumption, but in the I<very>
surprising case of you modifying @ISAs dynamically, you can call
clear_isa_scan to clear all the memoization.

You should not override this method.

=cut

my %isa_scan = ();
sub lex_refs { # report the lexicon references for this handle's class
  no strict;
  my $class = ref($_[0]) || $_[0];
  print "Lex refs lookup on $class\n" if $Debug > 1;
  return @{$isa_scan{$class}} if exists $isa_scan{$class};  # memoization!

  my @lex_refs = ();
  my $seen_r = ref($_[1]) ? $_[1] : {};

  if(defined %{$class . "::Lexicon"}) {
    my $ref = \%{$class . "::Lexicon"};
    push @lex_refs, $ref;
    print "%" . $class . "::Lexicon contains ",
         scalar(keys %$ref), " entries\n" if $Debug;
  }

  # defined() returns 0 if the hash exists but is empty, but that's not
  #  a problem for us.

  # Implements depth(height?)-first recursive searching of superclasses
  foreach my $superclass (@{$class . "::ISA"}) {
    print " Super-class search into $superclass\n" if $Debug;
    next if $seen_r->{$superclass}++;
    push @lex_refs, &lex_refs($superclass, $seen_r);  # call myself
  }

  $isa_scan{$class} = \@lex_refs; # save for next time
  return @lex_refs;
}

=item the routine Locale::Maketext::clear_isa_scan

=item also: the routine $LH->clear_isa_scan

=item also: the routine CLASS->clear_isa_scan

This clears all of C<lex_ref>'s memoization of all @ISA trees.

You should not override this method.

=cut

sub clear_isa_scan { %isa_scan = (); return; }

###########################################################################
1;

=back

=cut

=head1 COPYRIGHT

Copyright 1999, Sean M. Burke C<sburke@netadventure.net>, all rights
reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 AUTHOR

Sean M. Burke, C<sburke@netadventure.net>

=cut

__END__
