-module(node_validation).
-export([geom/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

geom({struct, Props}) when is_list(Props) ->
    case lists:keyfind(<<"type">>, 1, Props) of
        {<<"type">>, GeomType} ->
            validate_type(GeomType, Props);
        _ ->
            {error, <<"no type specified">>}
    end;
geom(_) ->
    {error, <<"invalid geometry">>}.

-ifdef(TEST).
validate_geom_test_() ->
    [
     ?_assertEqual(ok,
		   geom({struct, [{<<"type">>, <<"sphere">>},
				  {<<"parameters">>, {struct, [
							       {<<"radius">>, 1.2}
							      ]}}]})),
     ?_assertEqual({error, {struct, [{<<"radius">>,<<"must be positive">>}]}},
		   geom({struct, [{<<"type">>, <<"sphere">>},
				  {<<"parameters">>, {struct, [
							       {<<"radius">>, -0.3}
							      ]}}]})),
     ?_assertEqual(ok,
		   geom({struct, [{<<"type">>, <<"union">>},
				  {<<"children">>, [<<"abc">>, <<"123">>]}]}))
    ].
-endif.



validate_type(<<"sphere">>, Props) ->
    case lists:keyfind(<<"parameters">>, 1, Props) of
	false ->
	    {error, <<"no parameters specified">>};
	{_, Parameters} ->
	    validate_parameters(Parameters, [{<<"radius">>, fun positive/1}])
    end;
validate_type(<<"cuboid">>, Props) ->
    case lists:keyfind(<<"parameters">>, 1, Props) of
	false ->
	    {error, <<"no parameters specified">>};
	{_, Parameters} ->
	    validate_parameters(Parameters, [{<<"width">>, fun positive/1},
					     {<<"depth">>, fun positive/1},
					     {<<"height">>, fun positive/1}
					    ])
    end;
validate_type(<<"cylinder">>, Props) ->
    case lists:keyfind(<<"parameters">>, 1, Props) of
	false ->
	    {error, <<"no parameters specified">>};
	{_, Parameters} ->
	    validate_parameters(Parameters, [{<<"radius">>, fun positive/1},
					     {<<"height">>, fun positive/1}
					    ])
    end;
validate_type(<<"union">>, Props) ->
    validate_boolean(Props);
validate_type(<<"subtract">>, Props) ->
    validate_boolean(Props);
validate_type(<<"intersect">>, Props) ->
    validate_boolean(Props);
validate_type(_, _) ->
    {error, <<"unknown geometry type">>}.

validate_boolean(Props) ->
    case lists:keyfind(<<"children">>, 1, Props) of
	false ->
	    {error, <<"no children specified">>};
	{_, Children} when is_list(Children) ->
	    ok;
	_ ->
	    {error, <<"invalid children">>}
    end.

    



-ifdef(TEST).
validate_type_test_() ->
    [
     ?_assertEqual(
        ok, 
        validate_type(<<"sphere">>, [{<<"parameters">>, 
				      {struct, [{<<"radius">>, 0.1}]}}])),
     ?_assertEqual(
        {error, {struct, [{<<"radius">>, <<"not found">>}]}}, 
        validate_type(<<"sphere">>, [{<<"parameters">>, 
				      {struct, []}}])),
     ?_assertEqual(
        {error, {struct, [{<<"radius">>, <<"must be positive">>}]}}, 
        validate_type(<<"sphere">>, [{<<"parameters">>, 
				      {struct, [{<<"radius">>, -4}]}}])),
     ?_assertEqual(
        {error, {struct, [{<<"width">>, <<"must be positive">>},
			  {<<"height">>, <<"must be positive">>}]}}, 
        validate_type(<<"cuboid">>, [{<<"parameters">>,
				      {struct, [{<<"height">>, -4},
						{<<"depth">>, 3.1},
						{<<"width">>, -0.1}
					       ]}}]))
    ].
-endif.


validate_parameters(Parameters, Specs) ->
    Errors = lists:foldr(fun(Spec, Acc) ->
                                 case validate_spec(Parameters, Spec) of
                                     ok ->
                                         Acc;
                                     Error ->
                                         [Error|Acc]
                                 end
                         end,
                         [],
                         Specs),
    case Errors of
        [] ->
            ok;
        _ ->
            {error, {struct, Errors}}
    end.

-ifdef(TEST).
validate_parameters_test_() ->
    [
     ?_assertEqual(
        ok, 
        validate_parameters({struct, [{<<"a">>, 0.1},
                                      {<<"b">>, 3}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),

     ?_assertEqual(
        {error, {struct, [{<<"b">>, <<"must be positive">>}]}}, 
        validate_parameters({struct, [{<<"a">>, 0.1},
                                      {<<"b">>, -3}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ])),
     
     ?_assertEqual(
        {error, {struct, [{<<"a">>, <<"must be positive">>},
			  {<<"b">>, <<"must be positive">>}]}},
        validate_parameters({struct, [{<<"b">>, -0.6},
                                      {<<"a">>, -20}]},
                            [
                             {<<"a">>, fun positive/1},
                             {<<"b">>, fun positive/1}
                            ]))
    ].
-endif.

validate_spec({struct, ParameterProps}, {Field, CheckFn}) ->
    case lists:keyfind(Field, 1, ParameterProps) of
        false ->
            {Field, <<"not found">>};
        {_, Value} ->
            case CheckFn(Value) of
                ok ->
                    ok;
                {error, Reason} ->
                    {Field, Reason}
            end;
        _ ->
            ok
    end.

positive(Value) when is_integer(Value) andalso Value > 0 ->
    ok;
positive(Value) when is_float(Value) andalso Value > 0 ->
    ok;
positive(_) ->
    {error, <<"must be positive">>}.


-ifdef(TEST).
validate_spec_test_() ->
    [
     ?_assertEqual(ok, 
                   validate_spec({struct, [{<<"a">>, 0.1}]}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"must be positive">>}, 
                   validate_spec({struct, [{<<"a">>, 0}]}, {<<"a">>, fun positive/1})),
     ?_assertEqual({<<"a">>, <<"not found">>}, 
                   validate_spec({struct, []}, {<<"a">>, fun positive/1}))
    ].
-endif.

    


    

