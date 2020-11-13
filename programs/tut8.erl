%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 11月 2020 9:22
%%%-------------------------------------------------------------------
-module(tut8).
-author("12409").

%% API
-export([reverse/1]).

reverse([], NewList) ->
  NewList;
reverse([Head | Rest], NewList) ->
  reverse(Rest, [Head | NewList]).

reverse(List) ->
  reverse(List, []).


