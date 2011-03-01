-module(node_document_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([create/0, exists/1, load/1, update/2, ids/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

exists(Id) ->
    gen_server:call(?MODULE, {exists, Id}).

load(Id) ->
    gen_server:call(?MODULE, {load, Id}).

update(Id, GeomIds) ->
    gen_server:call(?MODULE, {update, Id, GeomIds}).

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
    DocId = node_uuid:uuid(),
    io:format("creating document ~p:~p~n", [DocId, []]),
    ok = save_to_file(DocId, []),
    {reply, DocId, State};
handle_call({exists, DocId}, _From, State) ->
    Reply = file_exists(DocId),
    {reply, Reply, State};
handle_call({load, DocId}, _From, State) ->
    io:format("loading document ~p~n", [DocId]),
    GeomIds = load_from_file(DocId),
    {reply, GeomIds, State};
handle_call({update, DocId, GeomIds}, _From, State) ->
    io:format("updating document ~p:~p~n", [DocId, GeomIds]),
    ok = save_to_file(DocId, GeomIds),
    {reply, ok, State};
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                          Private Functions                               %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filename(DocId) ->
    {ok, DbDir} = application:get_env(node, db_dir),
    filename:join(
      [filename:dirname(code:which(?MODULE)),
       DbDir, DocId ++ ".doc"]).

file_exists(DocId) ->
    filelib:is_regular(filename(DocId)).

load_from_file(DocId) ->
    {ok, Contents} = file:read_file(filename(DocId)),
    GeomIds = binary_to_term(Contents),
    lists:map(fun(GeomId) ->
                      node_geom_db:deserialize(GeomId)
              end,
              GeomIds),
    node_log:info("loaded geomIds: ~p~n", [GeomIds]),
    GeomIds.

save_to_file(DocId, GeomIds) ->
    ok = file:write_file(filename(DocId), term_to_binary(GeomIds)),
    node_log:info("serializing geom ids: ~p~n", [GeomIds]),
    lists:map(fun(GeomId) ->
                      S11N = node_geom_db:serialize(GeomId)
              end,
              GeomIds),
    ok.
