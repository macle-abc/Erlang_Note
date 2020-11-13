%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 11月 2020 16:34
%%%-------------------------------------------------------------------
-module(tut4).
-author("12409").

%% API
-export([list_length/1]).

list_length([]) -> 0;
list_length([Head | Other]) -> 1 + list_length(Other).