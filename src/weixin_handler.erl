%%%-------------------------------------------------------------------
%%% @author chenshb@mpreader.com
%%% @copyright (C) 2016, <MPR Reader>
%%% @doc
%%%
%%% @end
%%% Created : 08. 一月 2016 10:36
%%%-------------------------------------------------------------------
-module(weixin_handler).
-author("chenshb@mpreader.com").

-export([init/2]).
-include("markboy.hrl").

init(Req, Opt) ->
    {ok, "200", _Head, Body} = ibrowse:send_req("https://login.weixin.qq.com/jslogin?appid=wx782c26e4c19acffb&fun=new&lang=zh_CN&_=" ++ utils:to_list(utils:timestamp()), [], get),

    lager:info("~p", [Body]),
    {match, [[_ReturnCode, Uid]]} = re:run(utils:to_list(Body), "window\.QRLogin\.code = (.*); window\.QRLogin\.uuid = \"(.*)\";", [global, {capture, [1, 2], list}]),
    lager:info("~p", [Uid]),

    {ok,Module}=erlydtl:compile_file(markboy_misc:get_path(?WEIXIN_TPL), ?WEIXIN_TPL, []),
    {ok, Content} = Module:render([{img,utils:to_bin("https://login.weixin.qq.com/qrcode/" ++ utils:to_list(Uid))},{uid,Uid}]),
    Req2 = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Content, Req),
    {ok, Req2, Opt}.

