%%%-------------------------------------------------------------------
%%% @author Chenshaobo <chenshaobo65@gmail.com>
%%% @copyright (C) 2015, <MingChao>
%%% @doc
%%%
%%% @end
%%% Created : 17. 四月 2015 9:56
%%%-------------------------------------------------------------------
-author("Chenshaobo <chenshaobo65@gmail.com>").

-define(APP,markboy).
-define(HOME,home).
-define(HOME_TPL,"dtl/home.tpl").
-define(ARTICLE,article).
-define(ARTICLE_TPL,"dtl/article.tpl").
-define(ETS_MARKBOY_CACHE,ets_markboy_cache).

-define(DEBUG(C),io:format("~p:~p:~p~n",[?MODULE,?LINE,C])).
-define(DEBUG(X,Y),io:format( io_lib:format("~-15w:~-4w>",[?MODULE,?LINE]) ++ X ++"~n",Y)).