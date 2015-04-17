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
    {ok,Req2,Opt}.







