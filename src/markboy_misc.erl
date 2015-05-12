%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2015, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 17. 四月 2015 10:21
%%%-------------------------------------------------------------------
-module(markboy_misc).
-author("Chenshaobo <chenshaobo65@gmail.com>").
-include("markboy.hrl").
%% API
-export([
    get_priv_path/0,
    get_path/1,
    response/2,
    get_articles/0
]).

-define(PRIV_PATH,"/data/blog").

response(Req,?HOME)->
    Path=cowboy_req:path(Req),
    [NewestFile|_Remain]=MDFiles=get_articles(),
    NewestTime=filelib:last_modified(NewestFile),
    case markboy_cache:get(Path) of
        {Path,CacheTime,Content} when CacheTime >= NewestTime ->
            cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Content, Req);
        _ ->
            {ok,Module}=erlydtl:compile_file(get_path(?HOME_TPL),?HOME_TPL),
            ResultList= get_articles_result(MDFiles),
            {ok,Content}=Module:render([{result,ResultList}]),
            response(Req,Path,Content)
    end;
response(Req,?ARTICLE)->
    [Path]=cowboy_req:path_info(Req),
    MDFile=md_file_path(Path),
    NewestTime=filelib:last_modified(MDFile),
    case markboy_cache:get(Path) of
        {Path,CacheTime,Content} when CacheTime >= NewestTime ->
            cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Content, Req);
        _ ->
            {ok,Module}=erlydtl:compile_file(get_path(?ARTICLE_TPL), ?ARTICLE_TPL,
                [{custom_tags_dir,get_path("dlt")},{custom_tags_modules,[erlmarkdown]}]),
            {ok, Content} = Module:render([{path,MDFile}]),
            response(Req,Path,Content)
    end.


response(Req,Path,Content)->
    markboy_cache:set({Path,{erlang:date(),erlang:time()},Content}),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Content, Req).

get_articles_result(MDFiles)->
    [ begin
          BaseName=filename:basename(FileName,<<".md">>),
          {{Y,M,D},{H,Min,S}}=filelib:last_modified(FileName),
          [ <<"/",(unicode:characters_to_binary(BaseName,unicode))/binary >>, unicode:characters_to_binary(BaseName,unicode) ,io_lib:format("~4w-~4w-~4w   ~2w:~2w:~2w",[Y,M,D,H,Min,S])]
      end|| FileName <-MDFiles ].




get_priv_path()->
    ?PRIV_PATH.

get_path(FilePath)->
    Path=get_priv_path(),
    filename:join(Path,FilePath).

md_file_path(Path)->
    << (filename:join(get_md_path() ,unicode:characters_to_binary(Path,utf8)))/binary,".md" >>.

get_md_path()->
    filename:join(get_priv_path() ,<<"md">>).

get_articles()->
    Path=filename:join(get_md_path(), <<"*.md">>),
    lists:reverse(
        lists:sort(
            fun(A,B)->
                filelib:last_modified(A) =< filelib:last_modified(B)
            end,filelib:wildcard(binary_to_list(Path)))).