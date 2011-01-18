-module(node).
-export([start/1, stop/0, init/1]).
-export([send/1]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

send(X) when is_binary(X) ->
    call_port(X).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.
init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtPrg}, [stream]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
            io:format("SEND: ~p~n", [Msg]),
	    Port ! {self(), {command, Msg}},
	    receive
		{Port, {data, Data}} ->
                    io:format("RECV: ~p~n", [Data]),
		    Caller ! {complex, Data}
	    end,
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated);
        X ->
            io:format("received: ~p", [X]),
            loop(Port)
                
    end.
