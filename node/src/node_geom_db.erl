-module(node_geom_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([exists/1, raw_geom_record/1, create/1, update/2, geometry/1, tesselation/1, stl/1]).
-export([serialize/1, deserialize/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

exists(Id) ->
    gen_server:call(?MODULE, {exists, Id}).
raw_geom_record(Id) ->
    gen_server:call(?MODULE, {raw_geom_record, Id}).
create(Geometry) ->
    gen_server:call(?MODULE, {create, Geometry}, 30000).
update(Id, Geometry) ->
    gen_server:call(?MODULE, {update, Id, Geometry}, 30000).
geometry(Id) ->
    gen_server:call(?MODULE, {geometry, Id}, 30000).
tesselation(Id) ->
    gen_server:call(?MODULE, {tesselation, Id}, 30000).
stl(Id) ->
    gen_server:call(?MODULE, {stl, Id}, 30000).
serialize(Id) ->
    gen_server:call(?MODULE, {serialize, Id}, 30000).
deserialize(Id) ->
    gen_server:call(?MODULE, {deserialize, Id}, 30000).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(geom_doc, {geometry, tesselation}).

init([]) ->
    {ok, []}.

handle_call({exists, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, _} -> true;
                false -> false
            end,
    {reply, Reply, State};
handle_call({raw_geom_record, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, Record} -> Record;
                false -> undefined
            end,
    {reply, Reply, State};
handle_call({create, Geometry}, _From, State) ->
    {struct, GeomProps} = Geometry,
    {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
    Id = node_uuid:uuid(),
    Reply = case create_type(Id, GeomType, Geometry) of
                "\"ok\"" ->
                    Id;
                Error ->
                    {error, Error}
            end,
    {reply, Reply, [{Id, #geom_doc{ geometry = Geometry }}|State]};
handle_call({update, Id, Geometry}, _From, State) ->
    {struct, GeomProps} = Geometry,
    {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
    Reply = case create_type(Id, GeomType, Geometry) of
                "\"ok\"" ->
                    ok;
                Error ->
                    {error, Error}
            end,
    {reply, Reply, lists:keyreplace(Id, 1, State, {Id, #geom_doc{ geometry = Geometry }})};
handle_call({geometry, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, Record} -> 
                    Record#geom_doc.geometry;
                false -> 
                    undefined
            end,
    {reply, Reply, State};
handle_call({tesselation, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, Record} -> 
                    case Record#geom_doc.tesselation of
                        undefined ->
                            io:format("tesselating ~p~n", [Id]),
                            node_worker_server:call(
                              mochijson2:encode(
                                {struct, [{<<"tesselate">>, list_to_binary(Id)}]}));
                        T ->
                            T
                    end;
                false -> 
                    undefined
            end,
    {reply, Reply, State};
handle_call({stl, Id}, _From, State) ->
    Filename = "scratch/" ++ Id ++ ".stl",
    Msg = {struct, [{<<"type">>, <<"stl">>},
                    {<<"id">>, list_to_binary(Id)},
                    {<<"filename">>, list_to_binary(Filename)}
                   ]},
    %% TODO: Error handling
    "\"ok\"" = node_worker_server:call(mochijson2:encode(Msg)),
    {ok, STL} = file:read_file(Filename),
    {reply, STL, State};
handle_call({serialize, Id}, _From, State) ->
    Msg = {struct, [{<<"type">>, <<"serialize">>},
                    {<<"id">>, list_to_binary(Id)}]},
    {struct, [{<<"s11n">>, S11N}]} = mochijson2:decode(node_worker_server:call(mochijson2:encode(Msg))),

    {Id, Record} = lists:keyfind(Id, 1, State),
    GeomFilename = filename(Id),
    Geometry = Record#geom_doc.geometry,
    ToWrite = [{geom, Geometry},
               {s11n, S11N}],
    io:format("writing geometry for ~p to ~s~n", [Id, GeomFilename]),
    ok = file:write_file(GeomFilename, term_to_binary(ToWrite)),
    {reply, ok, State};
handle_call({deserialize, Id}, _From, State) ->
    GeomFilename = filename(Id),
    {ok, Contents} = file:read_file(GeomFilename),
    Props = binary_to_term(Contents),
    {geom, Geometry} = lists:keyfind(geom, 1, Props),
    {s11n, S11N} = lists:keyfind(s11n, 1, Props),
    
    %% Insert into worker
    Msg = {struct, [{<<"type">>, <<"deserialize">>},
                    {<<"id">>, list_to_binary(Id)},
                    {<<"s11n">>, S11N}]},
    "\"ok\"" = node_worker_server:call(mochijson2:encode(Msg)),
    
    NewState = case lists:keyfind(Id, 1, State) of
                   {_, _} ->
                       lists:keyreplace(Id, 1, State, {Id, #geom_doc{ geometry = Geometry }});
                   false ->
                       [{Id, #geom_doc{ geometry = Geometry }} | State]
               end,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                 private                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

filename(Id) ->
    {ok, DbDir} = application:get_env(node, db_dir),
    filename:join(
      [filename:dirname(code:which(?MODULE)),
       DbDir, Id ++ ".geom"]).

create_type(Id, <<"union">>, Geometry) ->
    create_boolean(Id, <<"union">>, Geometry);
create_type(Id, <<"subtract">>, Geometry) ->
    create_boolean(Id, <<"subtract">>, Geometry);
create_type(Id, <<"intersect">>, Geometry) ->
    create_boolean(Id, <<"intersect">>, Geometry);
%% Non-bool pass through
create_type(Id, _, Geometry) ->
    worker_create(Id, Geometry).

create_boolean(Id, Type, Geometry) ->
    {struct, GeomProps} = Geometry,
    {<<"children">>, Children} = lists:keyfind(<<"children">>, 1, GeomProps),
    Ids = lists:map(fun(Path) ->
                           "/geom/" ++ ChildId = binary_to_list(Path),
                           list_to_binary(ChildId)
                   end,
                   Children),
                   
    Transforms = case lists:keyfind(<<"transforms">>, 1, GeomProps) of
                     false -> [];
                     {<<"transforms">>, T} -> T
                 end,
    worker_create(Id, {struct, [{<<"type">>, Type},
                                {<<"children">>, Ids},
                                {<<"transforms">>, Transforms}
                               ]}).
worker_create(Id, Geometry) ->

    Msg = {struct, [{<<"type">>, <<"create">>},
                    {<<"id">>, list_to_binary(Id)},
                    {<<"geometry">>, Geometry}
                   ]},
    node_worker_server:call(mochijson2:encode(Msg)).

