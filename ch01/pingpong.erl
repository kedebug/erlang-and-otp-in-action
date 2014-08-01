-module(pingpong).

-export([run/0]).

run() ->
    Pid = spawn(fun pong/0),
    Pid ! self(),
    receive
        pong -> ok
    end.

pong() ->
    receive
        From -> From ! pong
    end.