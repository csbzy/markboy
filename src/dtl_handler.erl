
-module(dtl_handler).
-include("markboy.hrl").
-export([init/2]).

init(Req, Opt) ->
    Req2=markboy_misc:response(Req,?ARTICLE),
    NewValue = integer_to_list(random:uniform(1000000)),
    Req3 = cowboy_req:set_resp_cookie(
        <<"server">>, NewValue, [{path, <<"/">>}], Req2),
    {ok, Req3, Opt}.


