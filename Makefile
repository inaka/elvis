PROJECT = elvis_shell

DEPS = elvis_core getopt jiffy ibrowse egithub katana katana_code
SHELL_DEPS = sync
TEST_DEPS = katana_test mixer meck xref_runner
BUILD_DEPS = inaka_mk hexer_mk
DEP_PLUGINS = inaka_mk hexer_mk

dep_elvis_core  = git https://github.com/inaka/elvis_core      0.2.11
dep_getopt      = hex 0.8.2
dep_jiffy       = hex 0.14.7
dep_ibrowse     = hex 4.2.2
dep_egithub     = hex 0.2.2
dep_katana      = git https://github.com/inaka/erlang-katana   0.2.23
dep_katana_test = git https://github.com/inaka/katana-test     0.0.5
dep_katana_code = git https://github.com/inaka/katana-code     0.0.3
dep_sync        = git https://github.com/rustyio/sync          11df81d
dep_meck        = git https://github.com/eproxus/meck          0.8.4
dep_xref_runner = git https://github.com/inaka/xref_runner     0.2.6
dep_mixer       = git https://github.com/inaka/mixer           0.1.5
dep_inaka_mk    = git https://github.com/inaka/inaka.mk        1.0.0
dep_hexer_mk    = git https://github.com/inaka/hexer.mk        1.1.0

include erlang.mk

ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

CT_OPTS = -cover test/elvis.coverspec -erl_args -config config/test.config
SHELL_OPTS = -name elvis@`hostname` -s sync -s elvis -config config/elvis.config
ESCRIPT_NAME = elvis

# Builds the elvis escript.
escript::
	./elvis help

test-shell: build-ct-suites app
	erl -pa ebin -pa deps/*/ebin -name elvis-test@`hostname` -pa test -s sync -s elvis -config config/test.config

install: escript
	cp elvis /usr/local/bin
