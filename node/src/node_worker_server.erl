-module(node_worker_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(state, {port}).

init([]) ->
    ExtPrg = "../worker/build/worker",
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtPrg}, [{packet, 4}]),
    {ok, #state{port = Port}}.

handle_call(stop, _From, State) ->
    Reason = normal,
    Reply = stopped,
    {stop, Reason, Reply, State};
handle_call({call, Msg}, _From, State) ->
    io:format("SEND: ~p~n", [Msg]),
    Port = State#state.port,
    Port ! {self(), {command, Msg}},
    Reply = receive
                {Port, {data, Data}} ->
                    io:format("RECV: ~p~n", [Data]),
                    Data
            end,
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    io:format("UNKNOWN node_worker_server:handle_call(~p)~n", [Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#state.port->
    io:format("node_worker_server port has exited: ~p~n", [Reason]),
    {stop, port_process_terminated, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    io:format("node_worker_server:terminate(~p)~n", [Reason]),
    Port = State#state.port,
    Port ! {self(), close},
    receive 
        {Port, closed} ->
            ok;
        X ->
            throw(X)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



