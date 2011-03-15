-module(node_geom_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([exists/1, raw_geom_record/1, create/1, update/2, geometry/1, recursive_geometry/1]).
%-export([stl/1, serialize/1, deserialize/1]).


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

recursive_geometry(Id) ->
    gen_server:call(?MODULE, {recursive_geometry, Id}, 30000).

%% serialize(Id) ->
%%     gen_server:call(?MODULE, {serialize, Id}, 30000).

%% deserialize(Id) ->
%%     gen_server:call(?MODULE, {deserialize, Id}, 30000).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(geom_doc, {geometry}).

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
    Id = node_uuid:uuid(),
    NewState = [{Id, #geom_doc{ geometry = Geometry }}|State],
    {reply, {ok, Id}, NewState};
handle_call({update, Id, Geometry}, _From, State) ->
    NewState = lists:keyreplace(Id, 1, State, {Id, #geom_doc{ geometry = Geometry }}),
    {reply, ok, NewState};
handle_call({geometry, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, Record} -> 
                    {struct, GeomProps} = Record#geom_doc.geometry,
                    {struct, [{<<"id">>, list_to_binary(Id)}|
                              GeomProps]};
                false -> 
                    undefined
            end,
    {reply, Reply, State};
handle_call({recursive_geometry, Id}, _From, State) ->
    Reply = recursive_geometry(Id, State),
    {reply, Reply, State};

%% handle_call({serialize, Id}, _From, State) ->
%%     %%Msg = {struct, [{<<"type">>, <<"serialize">>},
%%     %%                {<<"id">>, list_to_binary(Id)}]},
%%     %%{struct, [{<<"s11n">>, S11N}]} = mochijson2:decode(node_worker_server:call(mochijson2:encode(Msg))),

%%     {Id, Record} = lists:keyfind(Id, 1, State),

%%     GeomFilename = geom_filename(Id),
%%     Geometry = Record#geom_doc.geometry,
%%     io:format("writing geometry for ~p to ~s~n", [Id, GeomFilename]),
%%     ok = file:write_file(GeomFilename, term_to_binary(Geometry)),

%%     %% Hash = node_hash:hash_geometry(recursive_geometry(Id, State)),
%%     %% OCCFilename = occ_filename(Hash),
%%     %% io:format("writing occ for ~p[~p] to ~s~n", [Id, Hash, OCCFilename]),
%%     %% ok = file:write_file(OCCFilename, term_to_binary(S11N)),

%%     {reply, ok, State};
%% handle_call({deserialize, Id}, _From, State) ->
%%     GeomFilename = geom_filename(Id),
%%     {ok, Contents} = file:read_file(GeomFilename),
%%     Geometry = binary_to_term(Contents),

%%     NewState = case lists:keyfind(Id, 1, State) of
%%                    {_, _} ->
%%                        lists:keyreplace(Id, 1, State, {Id, #geom_doc{ geometry = Geometry }});
%%                    false ->
%%                        [{Id, #geom_doc{ geometry = Geometry }} | State]
%%                end,

%%     %% Hash = node_hash:hash_geometry(recursive_geometry(Id, NewState)),
%%     %% OCCFilename = occ_filename(Hash),
%%     %% {ok, Contents2} = file:read_file(OCCFilename),
%%     %% S11N = binary_to_term(Contents2),
    
%%     %% Insert into worker
%%     %%Msg = {struct, [{<<"type">>, <<"deserialize">>},
%%     %%                {<<"id">>, list_to_binary(Id)},
%%     %%                {<<"s11n">>, S11N}]},
%%     %%"\"ok\"" = node_worker_server:call(mochijson2:encode(Msg)),
    
%%     {reply, ok, NewState};
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

geom_filename(Id) ->
    {ok, DbDir} = application:get_env(node, db_dir),
    filename:join(
      [filename:dirname(code:which(?MODULE)),
       DbDir, Id ++ ".geom"]).

recursive_geometry(Id, State) ->           
    Record = case lists:keyfind(Id, 1, State) of
                 false -> throw({child_not_found, Id});
                 {_, R} -> R
             end,
    {struct, Props1} = Record#geom_doc.geometry,
    Props2 = [{<<"id">>, list_to_binary(Id)}|
              Props1],
    case lists:keyfind(<<"children">>, 1, Props2) of
        false ->
            {struct, Props2};
        {<<"children">>, ChildIds} ->
            NewChildren = lists:map(fun(ChildIdBin) ->
                                            recursive_geometry(binary_to_list(ChildIdBin), State)
                                    end,
                                    ChildIds),
            Props3 = 
                lists:keyreplace(<<"children">>, 1, Props2, {<<"children">>, NewChildren}),
            {struct, Props3}
    end.                                    
