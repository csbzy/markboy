%%%-------------------------------------------------------------------
%%% @author chenshb@mpreader.com
%%% @copyright (C) 2016, <MPR Reader>
%%% @doc
%%%
%%% @end
%%% Created : 04. 一月 2016 9:13
%%%-------------------------------------------------------------------
-module(weixin_ws_handler).
-author("chenshb@mpreader.com").
-include_lib("xmerl/include/xmerl.hrl").
%% API
-export([init/2]).

-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-record(state, {}).

-define(UID, uid).
-define(SKEY,skey).
-define(WXSID,wxsid).
-define(WXUIN,wxuin).
-define(PASS_TICKET,pass_ticket).
-define(ISGRARYSCALE,is_grary_scale).









init(Req, HandlerOpts) ->
    lager:info("req:~p", [cowboy_req:qs(Req)]),
    lager:info("req:~p", [cowboy_req:parse_qs(Req)]),
    erlang:start_timer(1000, self(), <<"login">>),
    Map = cowboy_req:match_qs([{uid, ""}], Req),
    lager:info("get uid:~p", [Map]),
    erlang:put(?UID, maps:get(uid, Map)),
    {cowboy_websocket, Req, HandlerOpts}.



websocket_handle({text, <<"ping">>}, Req, State) ->
%%     lager:info("-----ping"),
    {reply, {text, <<"pong">>}, Req, State};
websocket_handle({ping, _Msg}, Req, State) ->
%%     lager:info("recevie ping:~p",[_Msg]),
    {reply, {pong, <<"pong">>}, Req, State};
websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, <<"That's what she said! ", Msg/binary>>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, <<"login">>}, Req, State) ->

    case login() of
        {true, LoginBody} ->
            {match, [[RedirectUrl]]} =
                re:run(utils:to_list(LoginBody), "window.redirect_uri=\"(.*)\";", [global, {capture, [1], list}]),
            lager:info("redirecturl:~p", [RedirectUrl]),
            {ok, "301", _Head, XMLBody} = ibrowse:send_req(RedirectUrl, [], get),
            lager:info("BODY:~p", [XMLBody]),
%%                 <error>
%%                 <ret>0</ret>
%%                 <message>OK</message>
%%                 <skey>@crypt_f37d6256_41d262ee851b2c7142f4cddcf919a597</skey>
%%             <wxsid>T0QNQFiJjBzo0R+z</wxsid>
%%             <wxuin>428718160</wxuin>
%%             <pass_ticket>yV0R3XTCNMGBJJVq7Tb01%2BLNSaL3J8TxXtAOWsh3vHM8ymCc7Kk4EWuKrePaL%2BZW</pass_ticket>
%%             %% <isgrayscale>1</isgrayscale>
%%             </error>
            {XmlElt, _} = xmerl_scan:string(XMLBody),
            [#xmlElement{content =[#xmlText{value = Ret}]} ] = xmerl_xpath:string("/error/ret", XmlElt),
            [#xmlElement{content =[#xmlText{value = SKey}]}] = xmerl_xpath:string("/error/skey", XmlElt),
            [#xmlElement{content =[#xmlText{value = Wxsid}]}] = xmerl_xpath:string("/error/wxsid", XmlElt),
            [#xmlElement{content =[#xmlText{value = Wxuin}]}] = xmerl_xpath:string("/error/wxuin", XmlElt),
            [#xmlElement{content =[#xmlText{value = PassTicket}] }] = xmerl_xpath:string("/error/pass_ticket", XmlElt),
            [#xmlElement{content =[#xmlText{value = Isgrayscale}]}] = xmerl_xpath:string("/error/isgrayscale", XmlElt),
            lager:info("~p", [{Ret, SKey, Wxsid, Wxuin, PassTicket, Isgrayscale}]),
            erlang:put(?SKEY,SKey),
            erlang:put(?WXSID,Wxsid),
            erlang:put(?WXUIN,Wxuin),
            erlang:put(?PASS_TICKET,PassTicket),
            erlang:put(?ISGRARYSCALE,Isgrayscale),
            {match, [[BaseUrl]]} =
                re:run(utils:to_list(RedirectUrl), "(.*)\/(.*)", [global, {capture, [1], list}]),

            InitUrl =BaseUrl ++ "/webwxinit?pass_ticket=" ++
                PassTicket ++ "&skey="++ SKey ++"&r=" ++ utils:to_list(utils:timestamp()),
            lager:info("init url:~p",[InitUrl]),
            JSON = json:encode({struct, [{'Uin',Wxuin},{'Sid',Wxsid},
                {'Skey',SKey},{'DeviceID','e000000000000000'}]}),
            {ok, "200", _, InitReturn} =
                ibrowse:send_req(InitUrl, [{"Content-Type", "application/json; charset=UTF-8"}], get,JSON),
            lager:info("ret:~p",[InitReturn]),


            GetContackUrl =BaseUrl ++ "/webwxgetcontact?pass_ticket=" ++
                PassTicket ++ "&skey="++ SKey ++"&r=" ++ utils:to_list(utils:timestamp()),
            lager:info("GetContackUrl url:~p",[GetContackUrl]),
            {ok, "200", _, GetContactReturn} =
                ibrowse:send_req(GetContackUrl, [{"Content-Type", "application/json; charset=UTF-8"}], get),
            lager:info("ret:~p",[GetContactReturn]),


            {reply, {text, <<"login">>}, Req, State};
        _ ->
            erlang:start_timer(1000, self(), <<"login">>),
            {ok, Req, State}
    end;

websocket_info(_Info, Req, State) ->
    lager:error("receive unknow msg:~p", [_Info]),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    <<"not auth">>.



login() ->
    Uid = erlang:get(?UID),
    Url = "https://login.weixin.qq.com/cgi-bin/mmwebwx-bin/login?tip=1&uuid="
        ++ utils:to_list(Uid) ++ "&_=" ++ utils:to_list(utils:timestamp()),
    lager:info("url:~p", [Url]),
    {ok, "200", _Head, Body} = ibrowse:send_req(Url ++ utils:to_list(utils:timestamp()), [], get),
    lager:info("BODY:~p", [Body]),
    {match, [[ReturnCode]]} =
        re:run(utils:to_list(Body), "window.code=(.*);", [global, {capture, [1], list}]),
    lager:info("~p", [ReturnCode]),
    {ReturnCode =:= "200", Body}.
