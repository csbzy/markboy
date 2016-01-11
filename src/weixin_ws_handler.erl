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
-export([response/2]).

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
    erlang:send_after(1000, self(), <<"login">>),
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

-define(MAX_USER_NUM,20).
websocket_info(<<"login">>, Req, State) ->
    case login() of
        {true, LoginBody} ->

            erlang:send_after(10,self(),{<<"get contact">>,LoginBody}),
            {reply, {text,response(0,<<"login ok">>)}, Req, State};
        _ ->
            erlang:send_after(1000, self(), <<"login">>),
            {ok, Req, State}
    end;
websocket_info({<<"get contact">>,LoginBody},Req,State)->
    {match, [[RedirectUrl]]} =
        re:run(utils:to_list(LoginBody), "window.redirect_uri=\"(.*)\";", [global, {capture, [1], list}]),
    lager:info("redirecturl:~p", [RedirectUrl]),
    {ok, "301", _Head1, XMLBody} = ibrowse:send_req(RedirectUrl, [], get),
    lager:info("~p,BODY:~p", [_Head1,XMLBody]),
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
    erlang:put(baseurl,BaseUrl),
    InitUrl =BaseUrl ++ "/webwxinit?pass_ticket=" ++
        PassTicket ++ "&skey="++ SKey ++"&r=" ++ utils:to_list(utils:timestamp()),
    lager:info("init url:~p",[InitUrl]),
    BaseRequest = {struct, [{'BaseRequest',{struct,[{'Uin',utils:to_int(Wxuin)},{'Sid',utils:to_bin(Wxsid)},
        {'Skey',utils:to_bin(SKey)},{'DeviceID',get_random_id()}]}}]},
    erlang:put(base_request,BaseRequest),
    BaseRequestJSON = json:encode(BaseRequest),

    lager:info("url:~p;body:~p",[InitUrl,utils:to_bin(BaseRequestJSON)]),
    Header2 = [{"Cookie",Data} || {Cookie,Data} <- _Head1 ,Cookie =:= "Set-Cookie"],
    PostHeader = [{"Content-Type", "application/json; charset=UTF-8"}|Header2],
    erlang:put(post_header,PostHeader),
    lager:info("~p",[Header2]),
    {ok, "200", _Head2, InitReturn} =
        ibrowse:send_req(InitUrl,PostHeader , post,BaseRequestJSON),
    lager:info("init ret :~p",[InitReturn]),


    GetContackUrl =BaseUrl ++ "/webwxgetcontact?pass_ticket=" ++
        PassTicket ++ "&skey="++ SKey ++"&r=" ++ utils:to_list(utils:timestamp()),
    lager:info("GetContackUrl url:~p",[GetContackUrl]),
    {ok, "200", _, GetContactReturn} =
        ibrowse:send_req(GetContackUrl, PostHeader, get),
    {struct,Proplist} = json:decode(GetContactReturn),
    ContactJson = proplists:get_value(<<"MemberList">>,Proplist),
    file:write_file("/tmp/1.log",GetContactReturn,[write,binary]),
    RealContacts=
        lists:filter(fun({struct,UserData})->
            VerifyFlag = proplists:get_value(<<"VerifyFlag">>,UserData),
            UserName  = proplists:get_value(<<"UserName">>,UserData),
            R = re:run(UserName,"@.*",[global, {capture, [0], list}]),
            if VerifyFlag > 0 ->
                false ;
                R =:= nomatch->
                    false;
                true ->
                    true
            end
        end,ContactJson),
    lager:info("contact conunt :~p\n real contact count:~p",[length(ContactJson),length(RealContacts)]),
    erlang:send_after(500,self(),{<<"get del user">>,RealContacts}),
    {reply,{text,response(0,<<"get contact ok ">>)},Req,State};
websocket_info({<<"get del user">>,RealContacts},Req,State)->
    {_Count,Groups}=
        lists:foldl(fun(Ele,{Index,[FirstGroup|Remain]=Acc})->
            if
                Index rem ?MAX_USER_NUM =:= 0 ->
                    {Index +1,[ [Ele] | Acc]};
                true ->
                    {Index +1,[[Ele|FirstGroup]|Remain]}
            end
        end,{1,[[]]},RealContacts),
    BaseUrl = erlang:get(baseurl),
    PassTicket = erlang:get(?PASS_TICKET),
    BaseRequest = erlang:get(base_request),
    PostHeader = erlang:get(post_header),
    DelUsers=
        lists:foldl(fun(GroupMember,Acc)->
            timer:sleep(20000),
            Url = BaseUrl ++ "/webwxcreatechatroom?pass_ticket=" ++ PassTicket ++"&r=" ++ utils:to_list(utils:timestamp()),
            CreatRoomJson = json:encode({ struct,[{'BaseRequest', BaseRequest},
                {'MemberCount', erlang:length(GroupMember)},
                {'MemberList',GroupMember},
                {'Topic', ""}]}),
            {ok, "200", _, CreateReturn} =
                ibrowse:send_req(Url,PostHeader , post,CreatRoomJson),
            {struct,GroupCreateJson}=json:decode(CreateReturn),
            {struct,BaseResponse}=proplists:get_value(<<"BaseResponse">>,GroupCreateJson),
            case proplists:get_value(<<"Ret">>,BaseResponse) of
                0 ->
                    MemberList  = proplists:get_value(<<"MemberList">>,GroupCreateJson),
                    ChatRoomName = proplists:get_value(<<"ChatRoomName">>,GroupCreateJson),
                    {DelUserNameList1,UserNameList1}=
                        lists:foldl(fun({struct,MemberInfo},{DelUserNameList,UserNameList})->
                            case proplists:get_value(<<"MemberStatus">>,MemberInfo) of
                                4 ->
                                    {[ proplists:get_value(<<"UserName">>,MemberInfo) | DelUserNameList],
                                        [ proplists:get_value(<<"UserName">>,MemberInfo)|UserNameList]};
                                _ ->
                                    {DelUserNameList,[ proplists:get_value(<<"UserName">>,MemberInfo)|UserNameList]}
                            end end,{[],[]},MemberList),
                    DelUrl = BaseUrl ++ "/webwxcreatechatroom?pass_ticket=" ++ PassTicket,
                    DelMemberJson = json:encode({struct,[{'BaseRequest',BaseRequest},
                        {'ChatRoomName', ChatRoomName},
                        {'DelMemberList', UserNameList1}]}),
                    {ok, "200", _, DelReturn} =
                        ibrowse:send_req(DelUrl,PostHeader , post,DelMemberJson),
                    lager:info("del return :~p",[DelReturn]),
                    lists:flatten([DelUserNameList1|Acc]);
                _r->
                    lager:info("create error:~p, Baseresponse:~p",[_r,BaseResponse]),
                    Acc
            end
        end,[],Groups),
    DelUserNames=
        lists:filter(fun({struct,Ele})->
            UserName = proplists:get_value(<<"UserName">>,Ele),
            lists:member(UserName,DelUsers)
        end,RealContacts),
    lager:info("del users:~p",[DelUsers]),
    lager:info("del user nick name :~p",[DelUserNames]),
    Return = {struct,[{delete_user,(lists:flatten(DelUserNames))},{delete_count,erlang:length(DelUserNames)}]},
    lager:info("return:~p",[Return]),
    {reply,{text,json:encode(Return)},Req,State};
websocket_info(_Info, Req, State) ->
    lager:error("receive unknow msg:~p", [_Info]),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    <<"not auth">>.



login() ->
    Uid = erlang:get(?UID),
    Url = "https://login.weixin.qq.com/cgi-bin/mmwebwx-bin/login?tip=0&uuid="
        ++ utils:to_list(Uid) ++ "&_=" ++ utils:to_list(utils:timestamp()),
    lager:info("url:~p", [Url]),
    {ok, "200", Head, Body} = ibrowse:send_req(Url ++ utils:to_list(utils:timestamp()), [], get),
    lager:info("Header :~p BODY:~p", [Head,Body]),
    {match, [[ReturnCode]]} =
        re:run(utils:to_list(Body), "window.code=(.*);", [global, {capture, [1], list}]),
    lager:info("~p", [ReturnCode]),
    {ReturnCode =:= "200", Body}.

get_random_id()->
    MD5 = utils:to_md5(utils:to_list(utils:timestamp())),
    <<Return:16/binary,_/binary>> = MD5,
    Return.

response(Code,Status)->
    ("{" ++ "\"ret\" : " ++ utils:to_list(Code) ++ ","  ++ "\"status\":" ++"\"" ++ utils:to_list(Status) ++ "\"" ++ "}").