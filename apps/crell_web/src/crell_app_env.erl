-module(crell_app_env).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).
-export([ handle_json/2
]).

-define(STATE,crell_app_env_state).
-record(?STATE,{app_name}).

init(Req, Opts) ->
	AppName = cowboy_req:binding(app_name,Req),
    {cowboy_rest, Req, #?STATE{ app_name = list_to_atom(binary_to_list(AppName)) }}.

content_types_provided(Req, State) ->
	{[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    Data = crell_server:calc_app_env(State#?STATE.app_name),
    Json = to_json(State#?STATE.app_name, Data),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

to_json(App, Data) ->
    ParsableData = parsable(Data),
    jsx:encode(ParsableData).

parsable({K, V}) ->
    parsable([{K, V}]);
parsable(Data) when is_list(Data) ->
    try
        list_to_binary(lists:flatten(io_lib:format("~p",[Data])))
    catch
        throw:{error, unsupported_format} ->
            %% use a clumbsy string response, as a last resort
            list_to_binary(lists:flatten(io_lib:format("~s",[Data])));
        C:E ->
            io:format("\n\nC:~p\nE:~p\n~p\n\n",[C,E,erlang:get_stacktrace()]),
            %% use a clumbsy string response, as a last resort
            list_to_binary(lists:flatten(io_lib:format("~p",[Data])))
    end.

parsable_key_value({Key, Value=[]}, Acc) when is_atom(Key) andalso is_list(Value) ->
    [{Key, []} | Acc];
parsable_key_value({Key, Value}, Acc) when is_atom(Key) andalso is_list(Value) ->
    case io_lib:printable_unicode_list(Value) andalso io_lib:printable_list(Value) of
        true ->
            [{Key, list_to_binary(Value)} | Acc];
        false ->
            case Value of
                L when is_list(L) ->
                   [{Key,
                    lists:reverse(
                        lists:foldl(fun parsable_key_value/2, [], L)
                    )
                   }];
                Other ->
                    io:format("Other : ~p\n\n",[Other]),
                    [{Key, Value} | Acc]
            end
    end;
parsable_key_value({Key, Value}, Acc) when is_atom(Key) andalso is_integer(Value) ->
    [{Key, Value} | Acc];
parsable_key_value({Key, Value}, Acc) when is_atom(Key) andalso is_atom(Value) ->
    [{Key, Value} | Acc];
parsable_key_value({Key, Value}, Acc) when is_atom(Key) andalso is_tuple(Value) ->
    % [{Key, lists:reverse(
    %     lists:foldl(fun parsable_key_value/2, [], [Value])
    % )} | Acc];
    [{Key , tuple_to_list(Value)} | Acc];
parsable_key_value({Key, Value}, Acc) when is_atom(Key) andalso is_binary(Value) ->
    parsable_key_value({Key, binary_to_list(Value)}, Acc);
parsable_key_value({Key, Value}, Acc) when is_list(Key) ->
    parsable_key_value({list_to_atom(Key), Value}, Acc);
parsable_key_value({Key, Value}, Acc) when is_integer(Key) ->
    parsable_key_value({integer_to_list(Key), Value}, Acc);
parsable_key_value({Key, Value}, Acc) when is_binary(Key) ->
    parsable_key_value({binary_to_list(Key), Value}, Acc);
%% wasn't planning on supporting anything other than proplists.
%% but here goes anyway. taking {a,b,c,d,e,f} to : {a,[b,c,d,e,f]}
%% for valid json
parsable_key_value(KV, Acc) when is_tuple(KV) andalso size(KV) > 2 ->
    [{element(1,KV), [ element(X,KV) || X <- lists:seq(2,size(KV)) ]}|Acc].
