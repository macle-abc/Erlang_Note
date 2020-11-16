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

7. 匿名变量_用来取值占位，允许多次绑定

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
## 发送消息
Receiver ! message 其中消息可以使用self()来发送 发送进程的pid
向receiver发送message

Receiver可以是pid也可以是注册后的进程名

## 接受消息
```erlang
receive
   pattern1 ->
       actions1;
   pattern2 ->
       actions2;
   ....
   patternN
       actionsN
end.
% eg
pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.
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

# 并发编程

each thread of execution is called a **process**.

Erlang中的执行线程不共享数据，这就是为什么称为进程

语法:

```erlang
 spawn（Module，Exported_Function，Arguments List） % 返回值为创建进程的pid， 其中Exported_Function必须是export出的函数
```

spawn的意思是产生

## 消息队列

每个进程具有自己的消息队列来接受消息。

每一个消息都进行模式匹配，满足条件的消息被消耗，所有模式都不匹配的消息被遗留在消息队列中，开始下一个消息的模式匹配，如果全部消息都不能匹配任一模式，那么该进程将会阻塞等待新的消息。

---

self()返回执行self()的进程的pid

 ## 注册进程名来传递消息

```erlang
register(some_atom, Pid) 
%eg 
register(pong, spawn(tut16, pong, []))
```

## 分布式编程

使用了非常基础的身份验证机制，在需要互相发送消息的系统上具有**相同的magic cookie**即可

在主目录下创建.erlang.cookie文件即可(具有可读权限)

1. win

   $HOME环境变量

2. Linux

   cd即可

在计算机上运行的每个Erlang系统都称为一个**Erlang节点**。

---

**不同节点之间关于进程名传递消息的注意点**
要想通过进程注册的名称向其它结点上的进程发送消息，这个时候，我们就不能再只用 registered_name 作为参数了，而需要使用元组 {registered_name,node_name} 作为注册进程的名称参数。***但是对于pid来说无需任何修改***

## 分布式例子

```erlang
-module(tut17).


%% API
-export([ping/2, pong/0, start_ping/1, start_pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished, % 需要指定register_name, 节点名
    io:format("ping finished~n", []);
ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,  % 使用pid的话，即使不同节点也能传递消息
            pong()
    end.

start_pong() ->
    register(pong, spawn(tut17, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut17, ping, [3, Pong_Node]).
```

### ping节点

erl -sname ping

tut17:start_ping(list_to_atom("pong@DESKTOP-1RVHEGQ")).

### pong节点

erl -sname pong

tut17:start_pong().

### spawn允许在其他节点启动新的进程

**但是需要其他节点已经启动了, er:erl -sname Other_Node**

```erlang
start(Ping_Node) ->
    register(pong, spawn(tut18, pong, [])),
    spawn(Ping_Node, tut18, ping, [3, node()]). % Ping_Node即为Other_Node
```

**但是输出的结果依旧会显示在启动该进程的节点中，可类比RPC**

