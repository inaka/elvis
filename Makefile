PROJECT = elvis

DEPS = lager sync getopt jiffy ibrowse aleppo zipper egithub katana
TEST_DEPS = meck

dep_lager = git https://github.com/basho/lager.git 2.0.3
dep_sync = git https://github.com/inaka/sync.git 0.1.3
dep_getopt = git https://github.com/jcomellas/getopt v0.8.2
dep_meck = git https://github.com/eproxus/meck 0.8.2
dep_jiffy = git https://github.com/davisp/jiffy 0.14.2
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse v4.1.2
dep_aleppo = git https://github.com/inaka/aleppo 0.9.1
dep_zipper = git https://github.com/inaka/zipper 0.1.2
dep_egithub = git https://github.com/inaka/erlang-github 0.1.7
dep_katana =  git git://github.com/inaka/erlang-katana 0.2.10

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS = -cover test/elvis.coverspec  -erl_args -config config/test.config

# Builds the elvis escript.
escript: all
	rebar escriptize
	./elvis help

shell: app
	erl -pa ebin -pa deps/*/ebin -name elvis@`hostname` -s sync -s elvis -s lager -config config/elvis.config

test-shell: build-ct-suites app
	erl -pa ebin -pa deps/*/ebin -pa test -s sync -s elvis -s lager -config config/elvis.config

install: escript
	cp elvis /usr/local/bin

quicktests: ERLC_OPTS = $(TEST_ERLC_OPTS)
quicktests: clean app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
