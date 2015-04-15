%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(markdown_middleware_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([{'_', [
        {"/css/[...]", cowboy_static, {priv_dir, markdown_middleware, "css"}},
%        {"/favicon.ico",cowboy_static, {priv_dir, markdown_middleware, ""}},
        {"/favicon.ico", cowboy_static, {file,"favicon.ico",[]}},
        {"/js/[...]", cowboy_static, {priv_dir, markdown_middleware, "js"}},
        {"/", home_handler, []},
        {"/[...]", cowboy_static, {priv_dir, markdown_middleware,""}}
    ]}]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]},
		{middlewares, [cowboy_router,dtl_handler, cowboy_handler]}
	]),
	markdown_middleware_sup:start_link().

stop(_State) ->
	ok.
