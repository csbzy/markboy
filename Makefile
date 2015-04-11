PROJECT = markdown_middleware
DEPS = cowboy erlydtl
dep_cowboy = git https://github.com/ninenines/cowboy master
dep_erlydtl = git https://github.com/erlydtl/erlydtl master
include erlang.mk

a1:
	cp -rf deps/*/ebin/* ebin