-module(node_tesselation_resource).
-export([
         init/1, 
         allowed_methods/2,
         resource_exists/2,
	 content_types_provided/2,
	 provide_content/2
        ]).


-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

init([]) -> {ok, #context{}}.

allowed_methods(ReqData, Context) -> 
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    Exists = node_document_db:exists(Id),
    {Exists, ReqData, Context#context{id = Id}}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Id = Context#context.id,
    Tesselation =  node_document_db:tesselation(Id),
    {Tesselation, ReqData, Context}.

