%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2015, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 14. 四月 2015 16:57
%%%-------------------------------------------------------------------
-module(home_handler).
-author("Chenshaobo <chenshaobo65@gmail.com>").

-include("markboy.hrl").
%% API
-export([init/2]).

init(Req,Opt)->
    Req2=markboy_misc:response(Req,?HOME),
    NewValue = integer_to_list(random:uniform(1000000)),
    Req3 = cowboy_req:set_resp_cookie(
        <<"server">>, NewValue, [{path, <<"/">>}], Req2),
    {ok,Req3,Opt}.







