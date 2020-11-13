%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 11月 2020 22:52
%%%-------------------------------------------------------------------
-module(tut3).
-author("12409").

%% API
-export([convert_length/1]).

convert_length({centimeter, X}) -> {inch, X / 2.54};
convert_length({inch, Y}) -> {centimeter, Y * 2.54}.
