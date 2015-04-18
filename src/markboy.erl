%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(markboy).
-behaviour(application).
-include("markboy.hrl").
%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
        {'_', [
            {"/css/[...]", cowboy_static, {priv_dir, markboy, "css"}},
            {"/favicon.ico", cowboy_static, {file, "favicon.ico", []}},
            {"/js/[...]", cowboy_static, {priv_dir, markboy, "js"}},
            {"/", home_handler, []},
            {"/[...]", dtl_handler, []}
        ]}
    ]),
{ok, _} = cowboy:start_http(http, 100, [{port, 80}], [
		{env, [{dispatch, Dispatch}]},
		{middlewares, [cowboy_router,cowboy_handler]}
	]),
	markboy_sup:start_link().

stop(_State) ->
	ok.
