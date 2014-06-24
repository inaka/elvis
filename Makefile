PROJECT = elvis

DEPS = lager

dep_lager = https://github.com/basho/lager.git master

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
