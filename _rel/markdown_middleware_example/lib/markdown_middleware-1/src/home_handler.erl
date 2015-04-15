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

%% API
-export([init/2]).

-define(HOME_PATH,"dtl/home.tpl").


init(Req,Opt)->
    Path=cowboy_req:get(path,Req),
    {ok,Module}=erlydtl:compile_file(resource_path(?HOME_PATH),home),
    ResultList=get_topic_list(dir_path(Path)),
    {ok,IoList}=Module:render([{result,ResultList}]),
    Req2= cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], IoList, Req),
    
    {ok,Req2,Opt}.

get_topic_list(Path)->
    [ begin
          BaseName=filename:basename(FileName,".md"),
          {{Y,M,D},{H,Min,S}}=filelib:last_modified(FileName),
         [ <<"/",(unicode:characters_to_binary(BaseName,unicode))/binary ,".html">>, unicode:characters_to_binary(BaseName,unicode) ,io_lib:format("~4w-~4w-~4w   ~2w:~2w:~2w",[Y,M,D,H,Min,S]) ]
      end|| FileName<-filelib:wildcard(Path)].

dir_path(Path)->
    File=resource_path(Path),
    binary_to_list(File) ++ "/*.md".

resource_path(Path) ->
    PrivDir = code:priv_dir(markdown_middleware),
    Path1=
        case Path of
            <<$/,Remain/binary>>->
                Remain;
            [$/|Remain]->
                Remain;
            _ ->
                Path
        end,
    filename:join([PrivDir, Path1]).
