%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 11月 2020 9:55
%%%-------------------------------------------------------------------
-module(tut9).
-author("12409").

%% API
-export([test_if/2]).
test_if(A, B) ->
  if
    A == 5 ->
      io:format("A == 5~n", []),
      a_equals_5;
    B == 6 ->
      io:format("B == 6~n", []),
      b_equals_6;
    A == 2, B == 3 ->                      %That is A equals 2 and B equals 3
      io:format("A == 2, B == 3~n", []),
      a_equals_2_b_equals_3;
    A == 1 ; B == 7 ->                     %That is A equals 1 or B equals 7
      io:format("A == 1 ; B == 7~n", []),
      a_equals_1_or_b_equals_7
  end.

