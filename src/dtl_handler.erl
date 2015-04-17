
-module(dtl_handler).
-include("markboy.hrl").
-export([init/2]).

init(Req, Opt) ->
    Req2=markboy_misc:response(Req,?ARTICLE),
    {ok, Req2, Opt}.


