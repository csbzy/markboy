%%%-------------------------------------------------------------------
%%% @author chenshb@mpreader.com
%%% @copyright (C) 2015, <MPR Reader>
%%% @doc
%%%
%%% @end
%%% Created : 17. 十二月 2015 14:53
%%%-------------------------------------------------------------------
-module(utils).
-author("chenshb@mpreader.com").

%% API
-export([incr_userid/0]).
-export([to_list/1]).
-export([to_int/1]).
-export([user_key/1]).
-export([gen_session/0]).
-export([to_bin/1]).
-export([to_md5/1]).
-export([user_post_key/1]).
-export([timestamp/0]).
-export([post_key/1]).
-export([sub_key/1]).
-export([following_key/1]).
-export([followers_key/1]).


incr_userid()->
    eredis_pools:incr(redis,"next_user_id").


to_list(Atom) when is_atom(Atom) -> erlang:atom_to_list(Atom);
to_list(Int) when is_integer(Int) -> erlang:integer_to_list(Int);
to_list(List) when is_list(List) -> List;
to_list(Bin) when is_binary(Bin) -> erlang:binary_to_list(Bin);
to_list(Tuple) when is_tuple(Tuple) ->erlang:tuple_to_list(Tuple);
to_list(_) ->
    erlang:throw(error_type).

to_int(Bin)when is_binary(Bin) -> erlang:binary_to_integer(Bin);
to_int(List)when is_list(List)->erlang:list_to_integer(List);
to_int(Int)when is_integer(Int)->
    Int;
to_int(undefined)->
    0;
to_int(_)->
    elang:throw(error_type).

following_key(UserID)->
    "following:" ++ utils:to_list(UserID).

followers_key(UserID)->
    "followers:" ++ utils:to_list(UserID).

user_key(UserID)->
    "user_id:" ++ utils:to_list(UserID).

user_post_key(UserID)->
    "user_posts:" ++ utils:to_list(UserID).

post_key(PostID)->
    "post:" ++utils:to_list(PostID).

sub_key(UserID)->
    "sub:" ++ utils:to_list(UserID).

timestamp()->
    {A,B,_}=os:timestamp(),
    A * 1000000 + B.

gen_session()->
    to_md5(io_lib:format("~p",[os:timestamp()])).


to_md5(Value) ->
    MD5Str = hex(erlang:md5(Value)),
    to_bin(MD5Str).


to_bin(Int) when is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_bin(Float) when is_float(Float)->
    erlang:float_to_binary(Float,[{decimals, 2}, compact]);
to_bin(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
to_bin(List) when is_list(List) ->
    erlang:list_to_binary(List);
to_bin(Binary) when is_binary(Binary) ->
    Binary;
to_bin(_) ->
    erlang:throw(error_type).


hex(Bin) when is_binary(Bin) ->
    MD5Str = hex(to_list(Bin)),
    to_bin(MD5Str);
hex(L) when is_list(L) ->
    lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
    [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I) -> [$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) -> $0 + I.