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

-record(context, {id, json_obj}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET', 'POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    case node_document_db:tesselation(Id) of
        undefined ->
            {"not found", ReqData, Context};
        Tesselation ->
            {Tesselation, ReqData, Context}
    end.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {"/geom/1", ReqData, Context}. 

accept_content(ReqData, Context) ->
    Id = node_document_db:create(Context#context.json_obj),
    Path = io_lib:format("/geom/~s", [Id]),
    io:format("created geometry: ~s~n", [Path]),
    ReqData1 = wrq:set_resp_body(
                 mochijson2:encode({struct, [{<<"path">>, iolist_to_binary(Path)}]}), ReqData),
    {true, ReqData1, Context}.


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

%% TODO: Geometry parameters must be floats, not ints
malformed_request(ReqData, Context, 'POST') ->
    Body = wrq:req_body(ReqData),
    try 
	JSONObj = mochijson2:decode(Body),
        {false, ReqData, Context#context{ json_obj = JSONObj }}
    catch
	A:B ->
            node_log:info("malformed request: ~p -> ~p:~p", [Body, A, B]),
	    {true, wrq:set_resp_body("invalid JSON", ReqData), Context}
    end.

