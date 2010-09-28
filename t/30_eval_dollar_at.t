use strict;
use warnings;

{
    package TEST;
    use base 'Locale::Maketext';
}

{
    package TEST::en;
    use base 'TEST';
    our %Lexicon = (
        _AUTO => 1,
    );
}

package main;
use strict;
use warnings;
use Test::More tests => 9;

my $lh = TEST->get_handle('en');
is($lh->maketext("This works fine"), "This works fine", "straight forward _AUTO string test");

my $err = eval {
   $lh->maketext('this is ] an error');
};
is($err, undef, "no return from eval");
like("$@", qr/Unbalanced\s'\]',\sin/ms, '$@ shows that ] was unbalanced');  

# _try_use doesn't pollute $@
$@ = '';
is(Locale::Maketext::_try_use("This::module::does::not::exist"), 0, "0 return if module is missing when _try_use is called");
is($@, '', '$@ is clean after failed _try_use');

# _try_use doesn't pollute $@ for valid call
$@ = '';
is(Locale::Maketext::_try_use("Locale::Maketext::Guts"), 1, "1 return using valid module Locale::Maketext::Guts");
is($@, '', '$@ is clean after failed _try_use');

# failure_handler_auto handles $@ locally.
{
    $@ = '';
    my $err = '';
    $lh->{failure_lex}->{"foo_fail"} = sub {die("fail message");};
    $err = eval {$lh->failure_handler_auto("foo_fail")};
    is($err, undef, "die event calling failure_handler on bad code");
    like($@, qr/^Error in maketexting "foo_fail":/ms, "\$@ is re-written as expected.");
}
