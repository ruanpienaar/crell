-module(crell_goanna_api_ws).

-include_lib("goanna/include/goanna.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-define(STATE, crell_goanna_api_ws).
-record(?STATE, { polling = false }).


init(Req, Opts) ->
    % erlang:register(crell_goanna_api_ws, self()),
    process_flag(trap_exit, true),
    % io:format("Opts : ~p~n", [Opts]),
    {cowboy_websocket, Req, #?STATE{}}.

websocket_handle({text, ReqJson}, Req, State) ->
    % io:format("State : ~p~n", [State]),
    case jsx:decode(ReqJson) of
        [{<<"get">>, <<"nodes">>}] ->
            Json = get_nodes_json(),
            {reply, {text, Json}, Req, State};

        [{<<"get">>, <<"active_traces">>}] ->
        Json = active_traces_json(),
            {reply, {text, Json}, Req, State};

        [{<<"get">>, <<"runtime_modules">>}] ->
            JsonRunMods = jsx:encode( [ {<<"runtime_modules">>,
                                        [ crell_web_utils:ens_bin(Mod) || Mod <- crell_server:runtime_modules() ]}
                                      ] ),
            {reply, {text, JsonRunMods}, Req, State};

        [{<<"trace">>, Details}] ->
            ok = trace(Details),
            Json = active_traces_json(),
            {reply, {text, Json}, Req, State};

        %% Use a countdown timer to only poll, for a while..
        [{<<"polling">>,<<"true">>}] ->
            poller(100, 100),
            {reply, {text, <<"ok">>}, Req, State#?STATE{ polling = true }};

        [{<<"polling">>,<<"false">>}] ->
            %% stop_poller(),
            {reply, {text, <<"ok">>}, Req, State#?STATE{ polling = false }};

        [{<<"fetch">>,_SizeBin}] ->
            % _SizeInt = list_to_integer(binary_to_list(SizeBin)),
            Json = all_traces_json(),
            {reply, {text, Json}, Req, State};

        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"remove_node">>},
         {<<"args">>,[Node]}] ->
            %% TODO: how will i handle errors?
            ok = goanna_api:remove_node(list_to_atom(binary_to_list(Node))),
            Json = get_nodes_json(),
            {reply, {text, Json}, Req, State};

        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"add_node">>},
         {<<"args">>,[Node, Cookie, Type]}] ->
            %% TODO: how will i handle errors?
             {ok, _GoannaNodePid} =
                  goanna_api:add_node(list_to_atom(binary_to_list(Node)),
                                      list_to_atom(binary_to_list(Cookie)),
                                      list_to_atom(binary_to_list(Type))
                                     ),
            Json = get_nodes_json(),
            {reply, {text, Json}, Req, State};

        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"runtime_module_functions">>},
         {<<"args">>,[Mod]}] ->
            %% TODO: maybe list * as an arity, so the front end user, can select, all arity.
            Functions = crell_server:runtime_module_functions(list_to_atom(binary_to_list(Mod))),
            Json = jsx:encode([{<<"functions">>, [ crell_web_utils:ens_bin(atom_to_list(F)++"/"++integer_to_list(Arity))
                || {F,Arity} <- Functions ]}]),
            {reply, {text, Json}, Req, State};

        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"stop_trace">>},
         {<<"args">>,[]}] ->
            io:format("Here 1~n", []),
            ok = goanna_api:stop_trace(),
            Json = active_traces_json(),
            {reply, {text, Json}, Req, State};

        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"stop_trace">>},
         {<<"args">>,[TrcPattern]}] ->
            [M, F, A] = trcpat_mfa_to_str(TrcPattern),
            %% TODO: handle * as all arity, so call, stop_trace(M,F)
            io:format("Here 2~n", []),
            ok = goanna_api:stop_trace(M, F, A),
            Json = active_traces_json(),
            {reply, {text, Json}, Req, State};

        UnknownJson ->
            io:format("UnknownJson: ~p~n", [UnknownJson]),
            Json = jsx:encode([{<<"unknown_json">>, UnknownJson}]),
            {reply, {text, Json}, Req, State}
    end;
websocket_handle(Data, Req, State) ->
    io:format("websocket_handle : ~p\n\n", [Data]),
    {ok, Req, State}.

websocket_info({timeout, _Ref, {<<"fetch">>, Count}}, Req, #?STATE{ polling = false } = State) ->
    Json = polling_end_json(),
    {reply, {text, Json}, Req, State};
websocket_info({timeout, _Ref, {<<"fetch">>, Count}}, Req, #?STATE{ polling = true } = State) ->
    NewCount = Count - 1,
    Json =
        case NewCount of
            0 ->
                polling_end_json();
            _ ->
                poller(500, Count-1),
                all_traces_json()
        end,
    {reply, {text, Json}, Req, State};
websocket_info(Info, Req, State) ->
    io:format("~p~n", [Info]),
    {ok, Req, State}.

poller(Ms, Count) when Count =< 0 ->
    ok;
poller(Ms, Count) when Count > 0 ->
    erlang:start_timer(Ms, self(), {<<"fetch">>, Count}).

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
    [F,A] = trcpat_fa_to_str(Fun),
    goanna_api:trace(crell_web_utils:ens_atom(Mod), F, A).
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

trcpat_mfa_to_str(Mfa) ->
    [M, F, A] = string:tokens(binary_to_list(Mfa), ":/"),
    [crell_web_utils:ens_atom(M), crell_web_utils:ens_atom(F), crell_web_utils:ens_int(A)].

trcpat_fa_to_str(Fa)->
    [F, A] = string:tokens(binary_to_list(Fa), "/"),
    [crell_web_utils:ens_atom(F), crell_web_utils:ens_int(A)].

get_nodes_json() ->
    Nodes = goanna_api:nodes(),
    jsx:encode(
        [{<<"nodes">>,
            [ [{<<"node">>, crell_web_utils:ens_bin(Node)},
               {<<"cookie">>, crell_web_utils:ens_bin(Cookie)},
               {<<"type">>, crell_web_utils:ens_bin(Type)}] || {Node,Cookie,Type} <- Nodes ]
        }]
    ).

active_traces_json() ->
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
            jsx:encode(
                [{<<"active_traces">>,
                    [ [
                        {<<"module">>,   M},
                        {<<"function">>, F},
                        {<<"arity">>,    A}
                      ] || {_, M,F,A} <- ActiveTraces ]
                }]
            ).

    all_traces_json() ->
        Traces = goanna_api:pull_all_traces(),
        jsx:encode( [{<<"traces">>, [ dbg_trace_format_to_json(T) || T <- Traces ]}] ).

    polling_end_json() ->
        jsx:encode([{<<"traces_polling_end">>, <<"true">>}]).