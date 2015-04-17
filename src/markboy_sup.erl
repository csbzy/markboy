%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(markboy_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [
        {markboy_cache, {markboy_cache, start_link, []},
            permanent, 5000, worker, [markboy_cache]}
    ],
	{ok, {{one_for_one, 10, 10}, Procs}}.
