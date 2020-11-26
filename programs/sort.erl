%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 11月 2020 23:11
%%%-------------------------------------------------------------------
-module(sort).
-author("12409").

%% API
-export([insert_sort/1]).

insert(Current_Number, [Head | Other]) ->
    if Current_Number =< Head ->
            [Current_Number] ++ [Head | Other];
       true ->
            [Head] ++ insert(Current_Number, Other)
    end;
insert(Current_Number, []) ->
    [Current_Number].

insert_sort(List) ->
    insert_sort(List, []).
insert_sort([], []) ->
    [];
insert_sort([], Sorted_List) ->
    Sorted_List;
insert_sort([Head | Other], Sorted_List) ->
    insert_sort(Other, insert(Head, Sorted_List)).
