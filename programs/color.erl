%%%-------------------------------------------------------------------
%%% @author 12409
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 11月 2020 16:42
%%%-------------------------------------------------------------------
-module(color).
-author("12409").

% 宏定义
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

%% API
-export([new/4, blend/2]).

new(R, G, B, A) when ?is_channel(R), ?is_channel(G),
  ?is_channel(B), ?is_channel(A) ->
  #{red => R, green => G, blue => B, alpha => A}.

blend(Src, Dst) ->
  blend(Src, Dst, alpha(Src, Dst)).

blend(Src, Dst, Alpha) when Alpha > 0.0 ->
  Dst#{
    red := red(Src, Dst) / Alpha,
    green := green(Src, Dst) / Alpha,
    blue := blue(Src, Dst) / Alpha,
    alpha := Alpha
  };
blend(_, Dst, _) ->
  Dst#{
    red := 0.0,
    green := 0.0,
    blue := 0.0,
    alpha := 0.0
  }.

% := 的语法用于提取对应key的value
% := 也可以用于更新(能够保证Dst必须含有上面四个key，否则语法错误)eg: blend/3
% 而直接使用=>将会直接覆盖并更新

alpha(#{alpha := SA}, #{alpha := DA}) -> SA + DA * (1.0 - SA).
red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
  SV * SA + DV * DA * (1.0 - SA).
green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
  SV * SA + DV * DA * (1.0 - SA).
blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
  SV * SA + DV * DA * (1.0 - SA).