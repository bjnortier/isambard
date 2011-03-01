-module(node_document_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").


%%--------------------------------------------------------------------
%% Test server callback functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> DefaultData
%% DefaultData: [tuple()]
%% Description: Require variables and set default values for the suite
%%--------------------------------------------------------------------
suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 create
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    %application:start(sasl),
    application:start(inets),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    ok.

create(_Config) ->
    %% Create
    {ok,{{"HTTP/1.1",200,_}, _, PostResponse}} = 
	httpc:request(post, {"http://localhost:8001/doc/", [], "application/json", "[]"}, [], []),
    {struct, [{<<"path">>, PathBin}]} = mochijson2:decode(PostResponse),
    
    %% Get
    DocPath = binary_to_list(PathBin),
    "/doc/" ++ DocId = DocPath,
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse}} = 
	httpc:request(get, {"http://localhost:8001" ++ DocPath, []}, [], []),
    [] = mochijson2:decode(GetResponse),

    %% Create a geometry node
    GeomProps = [{<<"type">>, <<"sphere">>},
                 {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}],
    GeomId = node_geom_db:create({struct, GeomProps}),

    %% Simulate saving the geometry node, which includes the path
    SaveDoc = [
               list_to_binary("/geom/" ++ GeomId)
              ],
    {ok,{{"HTTP/1.1",204,_}, _, SaveResponse}} = 
	httpc:request(put, {"http://localhost:8001" ++ DocPath, [], "application/json", iolist_to_binary(mochijson2:encode(SaveDoc))}, [], []),
    [] = SaveResponse,

    %% Restart the worker node to ensure it is empty, then load the document
    catch(node:stop()),
    catch(node:start()),
    timer:sleep(1000),
    false = node_geom_db:exists(GeomId),

    %% Load the document
    {ok,{{"HTTP/1.1",200,_}, _, LoadResponse}} = 
	httpc:request(get, {"http://localhost:8001" ++ DocPath, []}, [], []),
    SaveDoc = mochijson2:decode(LoadResponse),

    true = node_document_db:exists(DocId),
    true = node_geom_db:exists(GeomId),

    {struct, _} = mochijson2:decode(node_geom_db:tesselation(GeomId)),

    ok.
    
    
