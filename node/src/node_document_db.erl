-module(node_document_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([create/0, exists/1, doc/1, update/2, ids/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

exists(Id) ->
    gen_server:call(?MODULE, {exists, Id}).
doc(Id) ->
    gen_server:call(?MODULE, {doc, Id}).
update(Id, NewDoc) ->
    gen_server:call(?MODULE, {update, Id, NewDoc}).
create() ->
    gen_server:call(?MODULE, create).
ids() ->
    gen_server:call(?MODULE, ids).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call(ids, _From, State) ->
    {reply, State, State};
handle_call(create, _From, State) ->
    Id = node_uuid:uuid(),
    io:format("created document ~p:~p~n", [Id, []]),
    {reply, Id, [{Id, []}| State]};
handle_call({exists, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, _} -> true;
                false -> false
            end,
    {reply, Reply, State};
handle_call({doc, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, Doc} -> Doc;
                false -> undefined
            end,
    {reply, Reply, State};
handle_call({update, Id, NewDoc}, _From, State) ->
    NewState = lists:keyreplace(Id, 1, State, {Id, NewDoc}),
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
