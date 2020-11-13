%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 11月 2020 17:31
%%%-------------------------------------------------------------------
-module(tut6).
-author("12409").

%% API
-export([list_max/1]).

list_max([Head | Rest]) ->
  list_max(Rest, Head).

list_max([], Res) ->
  Res;
list_max([Head | Rest], Result_so_far) when Head > Result_so_far ->
  list_max(Rest, Head);
list_max([Head | Rest], Result_for_far) ->
  list_max(Rest, Result_for_far).
