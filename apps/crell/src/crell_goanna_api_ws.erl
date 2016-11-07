-module(crell_goanna_api_ws).

-include_lib("goanna/include/goanna.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).


init(Req, Opts) ->
    erlang:register(crell_goanna_api_ws, self()),
    process_flag(trap_exit, true),

    {cowboy_websocket, Req, Opts}.

websocket_handle({text, <<"fetch">>}, Req, State) ->
    % fetcher(25),
    {reply, {text, <<"ok">>}, Req, State};
% websocket_handle({text, Msg}, Req, State) ->
%     {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle({text, ReqJson}, Req, State) ->
    case jsx:decode(ReqJson) of
        [{<<"get">>, <<"nodes">>}] ->
            Json = get_nodes_json(),
            {reply, {text, Json}, Req, State};
        [{<<"get">>, <<"active_traces">>}] ->
            ActiveTracesFull = goanna_api:list_active_traces(),
            ActiveTraces =
                lists:foldl(fun({{Node,TrcPattern},Now,TrcOpts}, Acc) ->
                    case lists:keyfind(TrcPattern, 1, Acc) of
                        false ->
                            [{TrcPattern,
                              crell_web_utils:ens_bin(TrcPattern#trc_pattern.m),
                              crell_web_utils:ens_bin(undef_function(TrcPattern#trc_pattern.f)),
                              crell_web_utils:ens_bin(undef_arity(TrcPattern#trc_pattern.a))
                            }|Acc];
                        {TrcPattern,_,_,_} ->
                            Acc
                    end
                end, [], ActiveTracesFull),
            Json  = jsx:encode(
                [{<<"active_traces">>,
                    [ [
                        {<<"module">>,   M},
                        {<<"function">>, F},
                        {<<"arity">>,    A}
                      ] || {_, M,F,A} <- ActiveTraces ]
                }]
            ),
            {reply, {text, Json}, Req, State};
        [{<<"get">>, <<"runtime_modules">>}] ->
            JsonRunMods = jsx:encode( [ {<<"runtime_modules">>,
                                        [ crell_web_utils:ens_bin(Mod) || Mod <- crell_server:runtime_modules() ]}
                                      ] ),
            {reply, {text, JsonRunMods}, Req, State};
        [{<<"module_functions">>}] ->
            {reply, {text, <<"">>}, Req, State};

        [{<<"trace">>, Details}] ->
            ok = trace(Details),
            {reply, {text, <<"ok">>}, Req, State};

        [{<<"fetch">>,_SizeBin}] ->
            % _SizeInt = list_to_integer(binary_to_list(SizeBin)),
            Traces = goanna_api:pull_all_traces(),
            Json = jsx:encode( [{<<"traces">>, [ dbg_trace_format_to_json(T) || T <- Traces ]}] ),
            {reply, {text, Json}, Req, State};

        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"remove_node">>},
         {<<"args">>,[Node]}] ->
            %% TODO: how will i handle errors?
            goanna_api:remove_node(list_to_atom(binary_to_list(Node))),
            Json = get_nodes_json(),
            {reply, {text, Json}, Req, State};

        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"add_node">>},
         {<<"args">>,[Node, Cookie, Type]}] ->
            %% TODO: how will i handle errors?
            goanna_api:add_node(list_to_atom(binary_to_list(Node)),
                                      list_to_atom(binary_to_list(Cookie)),
                                      list_to_atom(binary_to_list(Type))
                                     ),
            Json = get_nodes_json(),
            {reply, {text, Json}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"runtime_module_functions">>},
         {<<"args">>,[Mod]}] ->
            Functions = crell_server:runtime_module_functions(list_to_atom(binary_to_list(Mod))),
            Json = jsx:encode([{<<"functions">>, [ crell_web_utils:ens_bin(atom_to_list(F)++"/"++integer_to_list(Arity))
                || {F,Arity} <- Functions ]}]),
            {reply, {text, Json}, Req, State};
        A ->
            io:format("~p~n", [A]),
            {reply, {text, <<"ok">>}, Req, State}
    end;
websocket_handle(Data, Req, State) ->
    io:format("Data : ~p\n\n", [Data]),
    {ok, Req, State}.


websocket_info({timeout, _Ref, <<"fetch">>}, Req, State) ->
    case goanna_api:pull_all_traces() of
        [] ->
            fetcher(1000);
        Traces ->
            fetcher(25)
    end,
    {reply, {text, <<"Traces...">>}, Req, State};
websocket_info(Info, Req, State) ->
    io:format("~p~n", [Info]),
    {ok, Req, State}.

fetcher() ->
    fetcher(1000).

fetcher(Ms) ->
    erlang:start_timer(Ms, self(), <<"fetch">>).

ensure_info(T) ->
    crell_web_utils:ens_bin(io_lib:format("~p", [T])).

ensure_extra(T) ->
    crell_web_utils:ens_bin(io_lib:format("~p", [T])).

dbg_trace_format_to_json({Now, {trace, Pid, Label, Info}}) ->
    [{<<"datetime">>, crell_web_utils:ens_bin(crell_web_utils:localtime_ms_str(Now))},
     {<<"type">>, crell_web_utils:ens_bin(trace)},
     {<<"pid">>, crell_web_utils:ens_bin(Pid)},
     {<<"label">>, crell_web_utils:ens_bin(Label)},
     {<<"info">>, ensure_info(Info)}];

dbg_trace_format_to_json({Now, {trace, Pid, Label, Info, Extra}}) ->
    [{<<"datetime">>, crell_web_utils:ens_bin(crell_web_utils:localtime_ms_str(Now))},
     {<<"type">>, crell_web_utils:ens_bin(trace_extra)},
     {<<"pid">>, crell_web_utils:ens_bin(Pid)},
     {<<"label">>, crell_web_utils:ens_bin(Label)},
     {<<"info">>, ensure_info(Info)},
     {<<"extra">>, ensure_extra(Extra)}];

dbg_trace_format_to_json({Now, {drop, NumDropped}}) ->
    [{<<"datetime">>, crell_web_utils:ens_bin(crell_web_utils:localtime_ms_str(Now))},
     {<<"type">>, crell_web_utils:ens_bin(drop)},
     {<<"dropped">>, crell_web_utils:ens_bin(NumDropped)}].

trace([{<<"mod">>,Mod},
       {<<"fun">>,<<"*">>}]) ->
    goanna_api:trace(crell_web_utils:ens_atom(Mod));
trace([{<<"mod">>,Mod},
       {<<"fun">>,Fun}]) ->
    [Func, Ara] = string:tokens(binary_to_list(Fun), "/"),
    goanna_api:trace(crell_web_utils:ens_atom(Mod), crell_web_utils:ens_atom(Func), crell_web_utils:ens_int(Ara)).
%%trace([{<<"mod">>,Mod},
%%       {<<"fun">>,Fun},
%%       {<<"ara">>,Arity},
%%       {<<"tim">>,_Time},
%%       {<<"mes">>,_Messages}]) ->
%%    goanna_api:trace(crell_web_utils:ens_atom(Mod), crell_web_utils:ens_atom(Fun), crell_web_utils:ens_int(Arity)).

undef_function(undefined) ->
    "*";
undef_function(F) ->
    F.

undef_arity(undefined) ->
    "*";
undef_arity(A) ->
    A.

proplist_to_json_ready(Props) ->
    lists:map(fun(KVPair) ->
        [KVPair]
    end, Props).


get_nodes_json() ->
    Nodes = goanna_api:nodes(),
    jsx:encode(
        [{<<"nodes">>,
            [ [{<<"node">>, crell_web_utils:ens_bin(Node)},
               {<<"cookie">>, crell_web_utils:ens_bin(Cookie)},
               {<<"type">>, crell_web_utils:ens_bin(Type)}] || {Node,Cookie,Type} <- Nodes ]
        }]
    ).