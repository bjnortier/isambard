-module(node_brep_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([exists/1, create/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

exists(Hash) ->
    gen_server:call(?MODULE, {exists, Hash}).

create(Hash, Geometry) ->
    gen_server:call(?MODULE, {create, Hash, Geometry}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call({exists, Hash}, _From, State) ->
    Msg = {struct, [{<<"exists">>, list_to_binary(Hash)}]},
    Reply = case node_worker_server:call(mochijson2:encode(Msg)) of
                "true" -> true;
                "false" -> false;
                X -> throw({unknown_exists_reponse, X})
            end,
    {reply, Reply, State};
handle_call({create, Hash, Geometry}, _From, State) ->
    {struct, GeomProps} = Geometry,
    {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
    "\"ok\"" = create_type(Hash, GeomType, Geometry),
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
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


create_type(Hash, <<"union">>, Geometry) ->
    create_boolean(Hash, <<"union">>, Geometry);
create_type(Hash, <<"subtract">>, Geometry) ->
    create_boolean(Hash, <<"subtract">>, Geometry);
create_type(Hash, <<"intersect">>, Geometry) ->
    create_boolean(Hash, <<"intersect">>, Geometry);
%% Non-bool pass through
create_type(Hash, _, Geometry) ->
    worker_create(Hash, Geometry).

create_boolean(Hash, Type, Geometry) ->
    {struct, GeomProps} = Geometry,
    {<<"children">>, ChildIds} = lists:keyfind(<<"children">>, 1, GeomProps),
    ChildHashes = lists:map(fun(ChildId) ->
                                    ChildHash = node_hash:hash_geometry(
                                                  node_geom_db:recursive_geometry(binary_to_list(ChildId))),
                                    list_to_binary(ChildHash)
                            end,
                            ChildIds),

    Transforms = case lists:keyfind(<<"transforms">>, 1, GeomProps) of
                     false -> [];
                     {<<"transforms">>, T} -> T
                 end,
    worker_create(Hash, {struct, [{<<"type">>, Type},
                                  {<<"children">>, ChildHashes},
                                  {<<"transforms">>, Transforms}
                                 ]}).
worker_create(Hash, Geometry) ->
    Msg = {struct, [{<<"type">>, <<"create">>},
                    {<<"id">>, list_to_binary(Hash)},
                    {<<"geometry">>, Geometry}
                   ]},
    node_worker_server:call(mochijson2:encode(Msg)).
