PROJECT = markboy
PROJECT_DESCRIPTION = busniess
PROJECT_VERSION = 0.0.1

CONFIG ?= config/sys.config
SHELL_OPTS = -s ${PROJECT} -config ${CONFIG}
# Options.
ERLC_OPTS +=  +'{parse_transform, lager_transform}'
# Dependencies.

DEPS = cowboy  lager sync  erlydtl ibrowse
dep_cowboy = git https://github.com/ninenines/cowboy master
dep_lager = git git://github.com/basho/lager.git master
dep_sync  = git git://github.com/rustyio/sync  master
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse master
dep_erlydtl = git https://github.com/erlydtl/erlydtl.git master
# Standard targets.

include erlang.mk

#config :
#    escript generate_config
# Compile options.
#COMPILE_FIRST +=  lager_transform
#ERLC_OPTS += +debug_info +warn_export_vars


# Generate rebar.config on build.

app:: rebar.config


# Also dialyze the tests.

# DIALYZER_OPTS += --src -r test

# Use erl_make_certs from the tested release.