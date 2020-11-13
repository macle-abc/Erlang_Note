# 简介

# 编码风格

1. 大驼峰加下划线来区分

# 语法
```erlang
-module(module_name). % 和文件名需要相同eg:module_name.erl
-export([]). % 只有导出的函数其他模块才能使用，否则只能该模块内部使用
```

1. 变量必须是大写字母开头

2. atom原子，小写字母开头，仅仅是名称不能具有类似变量的值，名称就是他们的值

   可以使用'atom'来表示atom或者'a t o m'这种本来不加''语法错误的原子

3. 元组{}推荐的编码风格采用{atomic, Data}用atomic来描述data的信息

4. 列表[] 使用|来切割list或者拼接到list开头

   ```erlang
   [One, Two | Other] = [1, 2]. 
   Other将会为[]
   One为1
   Two为2
   ```

   **erlang没有字符串的类型，而是可以用unicode字符列表来表示字符串，比如[97, 98, 99]可以表示"abc"**

5. 字典语法: #{key => value}

   ```erlang
   % := 的语法用于提取对应key的value
   % := 也可以用于更新(能够保证必须含有key，否则语法错误)eg: blend/3
   % 而直接使用=>将会直接覆盖并更新
   13> #{2 => 3}#{2 => 4, 3 => 5}.
   #{2 => 4,3 => 5}
   14> #{4 => 3}#{2 := 4, 3 => 5}.
   ** exception error: {badkey,2}
   ```

6. %作为注释

## 运算符

- <小于
- \>大于
- ==等于（会转换成同一类型）
- \> =大于或等于
- = <小于或等于
- / =不等于
- =:= 精确的等于（类型和数值）
- rem 获得余数

## 语句块

1. if

	```erlang
	if
    	Condition 1 ->
        	Action 1;
    	Condition 2 ->
        	Action 2;
    	Condition 3 ->
        	Action 3;
    	Condition 4 ->
        	Action 4;
        true ->
        	Action 5 % 尤其注意最后使用true来兜底，保证else的情况，且最后一个分支没有;号
	end
	% 返回结果就是Action
	```
	
2. case

   ```erlang
   case Length of
       {centimeter, X}  -> {inch, X / 2.54};
       {inch, Y} -> {centimeter, Y * 2.54}
   end.
   ```

## 高阶函数

fun

当需要将函数作为参数来传递的时候语法为fun Func\_Name/Args\_Count

```erlang
convert_to_c({Name, {f, Temp}}) ->
  {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
  {Name, {c, Temp}}.

convert_list_to_c(List) ->
  lists:map(fun convert_to_c/1, List).
```

---

```erlang
86> Xf = fun(X) -> X * 2 end.
#Fun<erl_eval.5.123085357>
87> Xf(5).
10
4> lists:map(fun(X) when X > 3 -> X + 3; (X) -> X - 3 end, [1, 2, 3, 4]).
[-2,-1,0,7]
```

# 顺序编程

## 终端操作
erl来启动shell
退出终端q(). halt(). ctrl+c

在终端中编译代码
c(module_name).

v(line_no).可以查看历史的错误信息

## 内建函数(属于erlang模块，无需显示使用erlang:)

1. trunc(5.3) 结果为5 
2. round()
3. length()
4. float()
5. is_atom()
6. is_tuple()
7. atom_to_list()
8. list_to_atom()
9. integer_to_list()
10. 