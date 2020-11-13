%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 11月 2020 9:58
%%%-------------------------------------------------------------------
-module(tut10).
-author("12409").

%% API
-export([convert_length/1]).

convert_length(Length) ->
  case Length of
    {centimeter, X}  -> {inch, X / 2.54};
    {inch, Y} -> {centimeter, Y * 2.54}
  end.