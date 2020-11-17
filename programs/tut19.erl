-module(tut19).

-export([start_ping/1, start_pong/0, ping/2, pong/0]).

ping(0, Pong_Node) ->
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
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    after 5000 -> % 单位是ms, 如果收到{ping, Ping_PID}消息那么会取消after，否则会执行，且after必须是最后
        % receive中其它所有消息的接收处理都优先于超时消息。如果有一个返回值为整数值的函数，我们可以在 after 后调用该函数以将其返回值设为超时时间值，如下所示：
        % after pong_timeout() ->%
        io:format("Pong timed out~n", [])
    end.

start_pong() ->
    register(pong, spawn(tut19, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut19, ping, [3, Pong_Node]).