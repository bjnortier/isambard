-module(node_master_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
         create,
         create_boolean
	].

init_per_suite(Config) ->
    ok = application:load(node),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    ok = application:unload(node),
    ok.

create(_Config) ->
    Geometry = {struct, [{<<"type">>, <<"sphere">>},
                         {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    Id = node_master:create_geom(Geometry),
    {struct, _} = node_master:mesh_geom(Id),
    ok.

create_boolean(_Config) ->
    Geometry1 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {ok, Id1} = node_master:create_geom(Geometry1),
    {struct, _} = node_master:mesh_geom(Id1),

    Geometry2 = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}},
                          {<<"transforms">>, [
                                              {struct, [{<<"type">>, <<"translate">>},
                                                        {<<"parameters">>, {struct, [{<<"dx">>, 0.5},
                                                                                     {<<"dy">>, 0.5},
                                                                                     {<<"dz">>, 0.5}]}}]}
                                             ]}]},
                          
    {ok, Id2} = node_master:create_geom(Geometry2),
    {struct, _} = node_master:mesh_geom(Id2),
    

    Geometry3 = {struct, [{<<"type">>, <<"union">>},
                          {<<"children">>, [
                                            list_to_binary(Id1),
                                            list_to_binary(Id2)
                                           ]}]},
    {ok, Id3} = node_master:create_geom(Geometry3),
    {struct, _} = node_master:mesh_geom(Id3),

    ok.

    

%% serialize(_Config) ->
%%     Geometry = {struct, [{<<"type">>, <<"sphere">>},
%%                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
%%     Id = node_geom_db:create(Geometry),

%%     LocalTimeBeforeSerialize = calendar:local_time(),
%%     ok = node_geom_db:serialize(Id),

%%     %% After serialization, both the geometry (using the id), and the OpenCASCADE
%%     %% serialized data (according to the hash of hte geometry), should have been persisted
%%     {ok, DbDir} = application:get_env(node, db_dir),
%%     GeomFilename = filename:join(
%%                      [filename:dirname(code:which(?MODULE)),
%%                       DbDir, Id ++ ".geom"]),

%%     {ok, GeomRecord} = file:read_file_info(GeomFilename),
%%     regular = GeomRecord#file_info.type,
%%     true = GeomRecord#file_info.mtime >= LocalTimeBeforeSerialize,

%%     Hash = node_hash:hash_geometry(Geometry),
%%     OCCFileName = filename:join(
%%                             [filename:dirname(code:which(?MODULE)),
%%                              DbDir, Hash ++ ".occ"]),

%%     {ok, OccRecord} = file:read_file_info(OCCFileName),
%%     regular = OccRecord#file_info.type,
%%     true = OccRecord#file_info.mtime >= LocalTimeBeforeSerialize,

%%     ok.
