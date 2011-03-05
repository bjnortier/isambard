-module(node_geom_resource_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{minutes,1}}].

all() ->
	[
	 nested_geoms
	].

init_per_suite(Config) ->
    ok = application:load(node),
    application:set_env(node, port, 8001),
    application:start(inets),
    ok = node:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(node),
    application:unload(node),
    ok.

nested_geoms(_Config) ->
    %% Create
    GeomJSONA = {struct, [{<<"type">>, <<"sphere">>},
                          {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
    {ok,{{"HTTP/1.1",200,_}, _, PostAResponse}} = 
	httpc:request(post, {"http://localhost:8001/geom/", [], "application/json", iolist_to_binary(mochijson2:encode(GeomJSONA))}, [], []),
    {struct, [{<<"path">>, PathABin}]} = mochijson2:decode(PostAResponse),
    "/geom/" ++ GeomIdA = binary_to_list(PathABin),

    GeomJSONB = {struct, [{<<"type">>, <<"cuboid">>},
                          {<<"parameters">>, {struct, [{<<"width">>, 1.0},
                                                       {<<"depth">>, 1.0},
                                                       {<<"height">>, 1.0}]}}]},
    
    {ok,{{"HTTP/1.1",200,_}, _, PostBResponse}} = 
	httpc:request(post, {"http://localhost:8001/geom/", [], "application/json", iolist_to_binary(mochijson2:encode(GeomJSONB))}, [], []),
    {struct, [{<<"path">>, PathBBin}]} = mochijson2:decode(PostBResponse),
    "/geom/" ++ GeomIdB = binary_to_list(PathBBin),


    GeomJSONC = {struct, [{<<"type">>, <<"union">>},
                          {<<"children">>, [list_to_binary("/geom/" ++ GeomIdA),
                                            list_to_binary("/geom/" ++ GeomIdB)]}]},
    {ok,{{"HTTP/1.1",200,_}, _, PostCResponse}} = 
	httpc:request(post, {"http://localhost:8001/geom/", [], "application/json", iolist_to_binary(mochijson2:encode(GeomJSONC))}, [], []),
    {struct, [{<<"path">>, PathCBin}]} = mochijson2:decode(PostCResponse),
    "/geom/" ++ GeomIdC = binary_to_list(PathCBin),

    %% Get non-recursive
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse1}} = 
	httpc:request(get, {"http://localhost:8001/geom/" ++ GeomIdC, []}, [], []),
    ExpectedResultGeomC = {struct, [{<<"path">>, list_to_binary("/geom/" ++ GeomIdC)},
                                    {<<"type">>, <<"union">>},
                                    {<<"children">>, [list_to_binary("/geom/" ++ GeomIdA),
                                                      list_to_binary("/geom/" ++ GeomIdB)]}]},
    ExpectedResultGeomC = mochijson2:decode(GetResponse1),

    %% Get recursive
    {ok,{{"HTTP/1.1",200,_}, _, GetResponse2}} = 
	httpc:request(get, {"http://localhost:8001/geom/" ++ GeomIdC ++ "?recursive=true", []}, [], []),
    ExpectedResponse = 
        {struct, [{<<"path">>, list_to_binary("/geom/" ++ GeomIdC)},
                  {<<"type">>, <<"union">>},
                  {<<"children">>, [
                                    {struct, [{<<"path">>, list_to_binary("/geom/" ++ GeomIdA)},
                                              {<<"type">>, <<"sphere">>},
                                              {<<"parameters">>, {struct, [{<<"radius">>, 1.0}]}}]},
                                    {struct, [{<<"path">>, list_to_binary("/geom/" ++ GeomIdB)},
                                              {<<"type">>, <<"cuboid">>},
                                              {<<"parameters">>, {struct, [{<<"width">>, 1.0},
                                                                           {<<"depth">>, 1.0},
                                    {<<"height">>, 1.0}]}}]}
                                   ]}]},
    ExpectedResponse =  mochijson2:decode(GetResponse2),

    ok.
    
