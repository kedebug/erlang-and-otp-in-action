-module(ti_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1124).

start(_StartType, _StartArgs) ->
    {ok, LSock} = gen_tcp:listen(?DEFAULT_PORT, [{active, true}]),
    case ti_sup:start_link(LSock) of
        {ok, Pid} ->
            ti_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.