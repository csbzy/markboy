%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(dtl_handler).

-export([init/2]).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
    io:format("~w",[Req]),
	Req2 = echo(Method, Req),
	{ok, Req2, Opts}.


echo(_, Req) ->
	Path=cowboy_req:get(path,Req),
    io:format("~ts",[Path]),
    {ok, Module}=erlydtl:compile_file(priv_path(markdown_middleware,"dtl/base.tpl"),home_dtl),
    {ok, IOList} =Module:render([]),
    cowboy_req:reply(200,[{<<"content-type">>, <<"text/html">>}], IOList, Req).


priv_path(App, Path) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            error({badarg, "Can't resolve the priv_dir of application "
                ++ atom_to_list(App)});
        PrivDir when is_list(Path) ->
            PrivDir ++ "/" ++ Path;
        PrivDir when is_binary(Path) ->
            << (list_to_binary(PrivDir))/binary, $/, Path/binary >>
    end.