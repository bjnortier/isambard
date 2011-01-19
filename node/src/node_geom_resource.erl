-module(node_geom_resource).
-export([
         init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
         content_types_accepted/2,
         accept_content/2,
	 post_is_create/2,
	 create_path/2,
	 resource_exists/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) -> 
    {['GET', 'POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    %% %% Tesseletion = node_worker:send(mochijson2:encode({struct, [{<<"type">>, <<"create">>}, 
    %% %%                                                            {<<"id">>, <<"ab23b32b">>}, 
    %% %%                                                            {<<"geometry">>, {struct, [{<<"type">>, <<"cuboid">>}, 
    %% %%                                                                                       {<<"width">>, 1.0},
    %% %%                                                                                       {<<"depth">>, 1.0},
    %% %%                                                                                       {<<"height">>, 1.0}
    %% %%                                                                                      ]}
    %% %%                                                            }]})),
    Tesselation = node_worker:send(mochijson2:encode({struct, [{<<"type">>, <<"create">>}, 
                                                               {<<"id">>, <<"1kj1h2">>}, 
                                                               {<<"geometry">>, {struct, [{<<"type">>, <<"sphere">>}, 
                                                                                          {<<"radius">>, 1.0}
                                                                                         ]}
                                                               }]})),
    {Tesselation, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {"/geom/1", ReqData, Context}. 

accept_content(ReqData, Context) ->
    ReqData1 = wrq:set_resp_body(mochijson2:encode({struct, [{<<"path">>, <<"/geom/1kj1h2">>}]}), ReqData),
    {true, ReqData1, Context}.



