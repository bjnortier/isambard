-module(node_geom_db_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 serialize
	].

init_per_suite(Config) ->
    ok = application:load(node),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    ok = application:unload(node),
    ok.

serialize(_Config) ->
    Geometry = {struct, [{<<"type">>, <<"sphere">>},
                         {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    Id = node_geom_db:create(Geometry),

    LocalTimeBeforeSerialize = calendar:local_time(),
    ok = node_geom_db:serialize(Id),

    %% After serialization, both the geometry (using the id), and the OpenCASCADE
    %% serialized data (according to the hash of hte geometry), should have been persisted
    {ok, DbDir} = application:get_env(node, db_dir),
    GeomFilename = filename:join(
                     [filename:dirname(code:which(?MODULE)),
                      DbDir, Id ++ ".geom"]),

    {ok, GeomRecord} = file:read_file_info(GeomFilename),
    regular = GeomRecord#file_info.type,
    true = GeomRecord#file_info.mtime >= LocalTimeBeforeSerialize,

    Hash = node_hash:hash_geometry(Geometry),
    OCCFileName = filename:join(
                            [filename:dirname(code:which(?MODULE)),
                             DbDir, Hash ++ ".occ"]),

    {ok, OccRecord} = file:read_file_info(OCCFileName),
    regular = OccRecord#file_info.type,
    true = OccRecord#file_info.mtime >= LocalTimeBeforeSerialize,

    ok.
