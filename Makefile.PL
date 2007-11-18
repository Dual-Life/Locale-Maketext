require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME            => 'Locale-Maketext',
    VERSION_FROM    => 'lib/Locale/Maketext.pm',
    ABSTRACT_FROM   => 'lib/Locale/Maketext.pod',
    PREREQ_PM       => {
        'I18N::LangTags'   => 0.30,
        'Test::More'       => 0,
    },
    dist             => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    ($] < 5.008) ? () : ( INSTALLDIRS => 'perl' ),
    # If under a version with Maketext in core, overwrite that core file.
);


sub MY::postamble {
    return <<'MAKE_FRAG';
.PHONY: tags critic

tags:
	ctags -f tags --recurse --totals \
		--exclude=blib \
		--exclude=.svn \
		--exclude='*~' \
		--languages=Perl --langmap=Perl:+.t \

critic:
	perlcritic -1 -q -profile perlcriticrc -statistics lib/ t/

MAKE_FRAG
}
