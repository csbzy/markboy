#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose  -s
-mode(native).
-define(APP,markboy).
-define(HOME,home).
-define(HOME_TPL,"dtl/home.tpl").
-define(ARTICLE,article).
-define(ARTICLE_TPL,"dtl/article.tpl").
-define(ETS_MARKBOY_CACHE,ets_markboy_cache).

-define(DEBUG(C),io:format("~p:~p:~p~n",[?MODULE,?LINE,C])).
-define(DEBUG(X,Y),io:format( io_lib:format("~-15w:~-4w>",[?MODULE,?LINE]) ++ X ++"~n",Y)).

main(_Mods)->

    io:format("Begin to build html~n"),
    MDFiles=get_articles(),
    {ok,Module}=erlydtl:compile_file(get_path(?HOME_TPL),?HOME_TPL),
    ResultList= get_articles_result(MDFiles),
    {ok,Content}=Module:render([{result,ResultList}]),
    file:write(home_name(),Content).



get_articles_result(MDFiles)->
    [ begin
          BaseName=filename:basename(FileName,<<".md">>),
          {{Y,M,D},{H,Min,S}}=filelib:last_modified(FileName),
          [ <<"/",(unicode:characters_to_binary(BaseName,unicode))/binary >>, unicode:characters_to_binary(BaseName,unicode) ,io_lib:format("~4w-~4w-~4w   ~2w:~2w:~2w",[Y,M,D,H,Min,S])]
      end|| FileName <-MDFiles ].




get_priv_path()->
    root_dir().

get_out_put_path()->
    filename:join(root_dir(),"html").

home_name()->
    get_out_put_path() ++ "/index.html" .

get_path(FilePath)->
    Path=root_dir(),
    filename:join(Path,FilePath).


get_md_path()->
    filename:join(get_priv_path() ,<<"md">>).

get_articles()->
    Path=filename:join(get_md_path(), <<"*.md">>),
    filelib:wildcard(binary_to_list(Path)).


%% 鑾峰彇root dir
root_dir() ->
    "/data/markboy/priv/".