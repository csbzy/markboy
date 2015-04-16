%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(dtl_handler).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    case cowboy_req:path_info(Req) of
        [Path]->
            case filename:extension(Path) of
                <<".html">> -> 
                    %maybe_generate_markdown(resource_path(Path)),
                    generate_html(resource_path(Path));
                _Ext -> ok
            end;
        _->
            ok
    end,
    {ok, Req, Env}.


%maybe_generate_markdown(Path) ->
 %   ModifiedAt = filelib:last_modified(source_path(Path)),
  %  
   %%% MiddleFile =middle_path(Path),
   % GeneratedAt = filelib:last_modified(MiddleFile),
    
   % case ModifiedAt > GeneratedAt of
    %    true -> 
     %       erlmarkdown:conv_file(source_path(Path), MiddleFile),
      %      generate_html(Path);
       % false -> io:format("do not need to conv_file"),ok
   % end.


generate_html(Path) ->
    ModifiedAt = filelib:last_modified(source_path(Path)),
    GeneratedAt = filelib:last_modified(Path),
    case ModifiedAt > GeneratedAt of
        true ->
            {ok,Module}=erlydtl:compile_file(resource_path("dtl/article.tpl"), home_dtl,
			[{debug_info,true},{custom_tags_dir,resource_path("dlt")},{custom_tags_modules,[erlmarkdown]}]),

            {ok, IOList} = Module:render([{path,source_path(Path)}]),
            write(Path,IOList);
        false->
            io:format("do not neet to conv_file"),ok
    end.

write(File, Text) ->
    _Return=filelib:ensure_dir(File),
    case file:open(File, [write]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Text]),
            file:close(Id);
        _ ->
            error
    end.
%% priv_path(App, Path) ->
%%     case code:priv_dir(App) of
%%         {error, bad_name} ->
%%             error({badarg, "Can't resolve the priv_dir of application "
%%                 ++ atom_to_list(App)});
%%         PrivDir when is_list(Path) ->
%%             PrivDir ++ "/" ++ Path;
%%         PrivDir when is_binary(Path) ->
%%             <<(list_to_binary(PrivDir))/binary, $/, Path/binary>>
%%     end.
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
    File=filename:join([PrivDir, Path1]),
    File.

%middle_path(Path)->
 %   <<(filename:rootname(Path))/binary, ".middle">>.

source_path(Path) ->
    <<(filename:rootname(Path))/binary, ".md">>.

