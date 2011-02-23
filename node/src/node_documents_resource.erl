-module(node_documents_resource).
-export([
         init/1, 
         allowed_methods/2,
	 content_types_provided/2,
	 provide_content/2,
	 post_is_create/2,
         content_types_accepted/2,
         accept_content/2,
	 create_path/2,
	 resource_exists/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET', 'POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true, ReqData, Context};
        _ ->
            {true, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    DocIds = node_document_db:ids(),
    JSON = lists:map(fun list_to_binary/1, DocIds),
    {mochijson2:encode(JSON), ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    Id = node_document_db:create(),
    {"/doc/" ++ Id, ReqData, Context#context{id = Id}}. 

content_types_accepted(ReqData, Context) ->
    {[{"application/json", accept_content}], ReqData, Context}.

accept_content(ReqData, Context) ->
    ReqData1 = wrq:set_resp_body("{\"path\": \"/doc/" ++ Context#context.id ++ "\"}", ReqData),
    {true, ReqData1, Context}.



