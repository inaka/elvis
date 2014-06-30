PROJECT = elvis

DEPS = lager sync getopt
TEST_DEPS = meck

dep_lager = https://github.com/basho/lager.git 2.0.3
dep_sync = https://github.com/rustyio/sync.git master
dep_getopt = https://github.com/jcomellas/getopt v0.8.2
dep_meck = https://github.com/eproxus/meck 0.8.2

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_SUITES = elvis rules
CT_OPTS = -cover test/elvis.coverspec  -erl_args -config config/test

# Builds the elvis escript.
escript: all
	rebar escriptize
	./elvis help
