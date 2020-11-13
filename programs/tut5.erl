%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 11月 2020 17:20
%%%-------------------------------------------------------------------
-module(tut5).
-author("12409").

%% API
-export([format_temps/1]).

format_temps([]) -> ok;
format_temps([City | Rest]) ->
  print_temp(convert_to_celsius(City)),
  format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->
  {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->
  {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
  io:format("~-15w ~w c~n", [Name, Temp]).