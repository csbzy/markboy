%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(markboy).
-behaviour(application).
-include("markboy.hrl").
%% API.
-export([start/2]).
-export([stop/1]).
-export([start/0]).

%% API.

start()->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(merl),
    ok = application:start(xmerl),
    ok = application:start(erlydtl),
    ok = application:start(markboy).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
        {'_', [
            {"/css/[...]", cowboy_static, {dir, "./priv/css"}},
            {"/favicon.ico", cowboy_static, {file, "favicon.ico", []}},
            {"/js/[...]", cowboy_static, {dir, "./priv/js"}},
            {"/", home_handler, []},
            {"/[...]", dtl_handler, []}
        ]}
    ]),
    Port=get_port(),
{ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
		{env, [{dispatch, Dispatch}]},
		{middlewares, [cowboy_router,cowboy_handler]}
	]),
	markboy_sup:start_link().

stop(_State) ->
	ok.

get_port()->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = 8080,
            Port;
        Other ->
            list_to_integer(Other)
    end.