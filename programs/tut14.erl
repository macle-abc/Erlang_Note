%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 11月 2020 14:40
%%%-------------------------------------------------------------------
-module(tut14).

-export([start/0, say_something/2]).

say_something(What, 0) ->
  done;
say_something(What, Times) ->
  io:format("~p~n", [What]),
  say_something(What, Times - 1).

start() ->
  spawn(tut14, say_something, [hello, 3]),
  spawn(tut14, say_something, [goodbye, 3]).