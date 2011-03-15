-module(node_master).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([create_geom/1, mesh_geom/1, stl/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

create_geom(Geometry) ->
    gen_server:call(?MODULE, {create_geom, Geometry}, 30000).

mesh_geom(Id) -> 
    gen_server:call(?MODULE, {mesh_geom, Id}, 30000).

stl(Id) ->
    gen_server:call(?MODULE, {stl, Id}, 30000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call({create_geom, Geometry}, _From, State) ->
    {ok, Id} = node_geom_db:create(Geometry),
    {reply, {ok, Id}, State};
handle_call({mesh_geom, Id}, _From, State) ->
    Geometry = node_geom_db:geometry(Id),
    RecursiveGeom = node_geom_db:recursive_geometry(Id),
    Hash = node_hash:hash_geometry(RecursiveGeom),

    %% Create BRep if it doesn't exist
    case node_brep_db:exists(Hash) of
        false -> 
            node_log:info("BREP not found. Creating BREP for ~p[~p]~n", [Id, Hash]),
            ok = node_brep_db:create(Hash, Geometry);
        true ->
            ok
    end,
            
    Reply = node_mesh_db:mesh(Hash),
    {reply, Reply, State};
handle_call({stl, Id}, _From, State) ->
    RecursiveGeom = node_geom_db:recursive_geometry(Id),
    Hash = node_hash:hash_geometry(RecursiveGeom),
    Reply = node_mesh_db:stl(Hash),
    {reply, Reply, State};
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
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
