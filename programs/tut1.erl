%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 11月 2020 22:37
%%%-------------------------------------------------------------------
-module(tut1).
-author("12409").

%% API
-export([fac/1, mult/2]).


fac(1) -> 1;
% 注意顺序，否则会出现警告fac(1)无法匹配，因为理论上N是可以接受任意参数的，
% 很可能erlang就是按照定义的顺序来依次进行模式匹配
fac(N) -> N * fac(N - 1).

mult(X, Y) -> X * Y.