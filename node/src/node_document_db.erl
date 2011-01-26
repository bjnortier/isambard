-module(node_document_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).
-export([raw_geom_record/1, create/1, update/2, tesselation/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

raw_geom_record(Id) ->
    gen_server:call(?MODULE, {raw_geom_record, Id}).

create(Geometry) ->
    gen_server:call(?MODULE, {create, Geometry}).
update(Id, Geometry) ->
    gen_server:call(?MODULE, {update, Id, Geometry}).

tesselation(Id) ->
    gen_server:call(?MODULE, {tesselation, Id}).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-record(geom_doc, {geometry, tesselation}).

init([]) ->
    {ok, []}.

handle_call({raw_geom_record, Id}, _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                {Id, Record} -> Record;
                false -> undefined
            end,
    {reply, Reply, State};
    
handle_call({create, Geometry}, _From, State) ->
    {struct, GeomProps} = Geometry,
    {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
    Id = uuid(),
    Tesselation = create_type(Id, GeomType, Geometry),
    {reply, Id, [{Id, #geom_doc{ geometry = Geometry,
                                 tesselation = Tesselation }}|State]};
handle_call({update, Id, Geometry}, _From, State) ->
    {struct, GeomProps} = Geometry,
    {<<"type">>, GeomType} = lists:keyfind(<<"type">>, 1, GeomProps),
    Tesselation = create_type(Id, GeomType, Geometry),
    {reply, Id, lists:keyreplace(Id, 1, State, {Id, #geom_doc{ geometry = Geometry,
                                                               tesselation = Tesselation }})};


handle_call({tesselation, Id} , _From, State) ->
    Reply = case lists:keyfind(Id, 1, State) of
                 {Id, Record} -> 
                     Record#geom_doc.tesselation;
                 false -> 
                     undefined
            end,
    {reply, Reply, State};

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

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

uuid() ->
    to_hex(crypto:rand_bytes(16)).

create_type(Id, <<"union">>, Geometry) ->
    create_boolean(Id, <<"union">>, Geometry);
create_type(Id, <<"subtract">>, Geometry) ->
    create_boolean(Id, <<"subtract">>, Geometry);
create_type(Id, <<"intersect">>, Geometry) ->
    create_boolean(Id, <<"intersect">>, Geometry);
%% Non-bool pass through
create_type(Id, _, Geometry) ->
    worker_create(Id, Geometry).

create_boolean(Id, Type, Geometry) ->
    {struct, GeomProps} = Geometry,
    {<<"parameters">>, {struct, ParamProps}} = lists:keyfind(<<"parameters">>, 1, GeomProps),
    {<<"a">>, PathA} = lists:keyfind(<<"a">>, 1, ParamProps),
    {<<"b">>, PathB} = lists:keyfind(<<"b">>, 1, ParamProps),
    "/geom/" ++ IdA = binary_to_list(PathA),
    "/geom/" ++ IdB = binary_to_list(PathB),
    worker_create(Id, {struct, [{<<"type">>, Type},
                                {<<"parameters">>, {struct,
                                                    [{<<"a">>, list_to_binary(IdA)},                                
                                                     {<<"b">>, list_to_binary(IdB)}]}}]}).
worker_create(Id, Geometry) ->
    Msg = {struct, [{<<"type">>, <<"create">>},
                    {<<"id">>, list_to_binary(Id)},
                    {<<"geometry">>, Geometry}
                   ]},
    node_worker_server:call(mochijson2:encode(Msg)).

