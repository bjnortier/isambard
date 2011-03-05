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
	 resource_exists/2,
         malformed_request/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

%% The incoming JSON is transformed to convert paths (e.g. "/geom/1") to
%% ids for children
-record(context, {id, geom_json}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET', 'POST', 'PUT'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true, ReqData, Context};
        _ ->
            Exists = node_geom_db:exists(Context#context.id),
            {Exists, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    case wrq:get_qs_value("recursive", "false" ,ReqData) of
        "false" ->
            Geometry = node_geom_db:geometry(Id),
            {mochijson2:encode(transform_ids_to_paths(Geometry)), ReqData, Context};
        "true" ->
            Geometry = node_geom_db:recursive_geometry(Id),
            {mochijson2:encode(transform_ids_to_paths(Geometry)), ReqData, Context}
    end.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    %% FIXME: This is not the correct path
    {"/geom/1", ReqData, Context}. 

accept_content(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            Id = node_geom_db:create(Context#context.geom_json),
            Path = io_lib:format("/geom/~s", [Id]),
            ReqData1 = wrq:set_resp_body(
                         mochijson2:encode({struct, [{<<"path">>, iolist_to_binary(Path)}]}), ReqData),
            {true, ReqData1, Context};
        'PUT' ->
            case node_geom_db:update(Context#context.id, Context#context.geom_json) of
                ok ->
                    {true, ReqData, Context};
                {error, Error} ->
                    io:format("ERR: ~p~n", [Error]),
                    {false, ReqData, Context}
            end
    end.


malformed_request(ReqData, Context) ->
    Method = wrq:method(ReqData),
    malformed_request(ReqData, Context, Method).
    

malformed_request(ReqData, Context, 'GET') ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {true, wrq:set_resp_body("missing id: /geom/<id>", ReqData), Context};
        Id when is_list(Id) ->
            {false, ReqData, Context#context{id = Id}}
    end;
malformed_request(ReqData, Context, 'PUT') ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {true, wrq:set_resp_body("missing id: /geom/<id>", ReqData), Context};
        Id when is_list(Id) ->
            malformed_json_request(ReqData, Context#context{id = Id})
    end;
malformed_request(ReqData, Context, 'POST') ->
    malformed_json_request(ReqData, Context).

malformed_json_request(ReqData, Context) ->
    Body = wrq:req_body(ReqData),
    try 
	JSON = mochijson2:decode(Body),
        {false, ReqData, Context#context{ geom_json = transform_paths_to_ids(JSON) }}
    catch
	A:B ->
            node_log:info("malformed request: ~p -> ~p:~p", [Body, A, B]),
	    {true, wrq:set_resp_body("invalid JSON", ReqData), Context}
    end.


transform_paths_to_ids(JSON = {struct, Props}) ->
    case lists:keyfind(<<"children">>, 1, Props) of
        false ->
            JSON;
        {<<"children">>, Paths} ->
            NewChildren = lists:map(fun(Path) ->
                                            "/geom/" ++ Id = binary_to_list(Path),
                                            list_to_binary(Id)
                                    end,
                                    Paths),
            {struct, lists:keyreplace(<<"children">>, 1, Props, {<<"children">>, NewChildren})}
    end.

transform_ids_to_paths({struct, Props}) ->
    {<<"id">>, IdBin} = lists:keyfind(<<"id">>, 1, Props),
    Props1 = lists:keyreplace(<<"id">>, 1, Props, 
                             {<<"path">>, list_to_binary("/geom/" ++ binary_to_list(IdBin))}),
    case lists:keyfind(<<"children">>, 1, Props1) of
        false ->
            {struct, Props1};
        {<<"children">>, IdsOrNestedStructs} ->
            NewChildren = lists:map(fun(Id) when is_binary(Id) ->
                                            list_to_binary("/geom/" ++ binary_to_list(Id));
                                       (Structs) when is_tuple(Structs) ->
                                            transform_ids_to_paths(Structs)
                                    end,
                                    IdsOrNestedStructs),
            {struct, lists:keyreplace(<<"children">>, 1, Props1, {<<"children">>, NewChildren})}
    end.




