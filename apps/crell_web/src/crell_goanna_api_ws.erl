-module(crell_goanna_api_ws).

-include_lib("goanna/include/goanna.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-define(STATE, crell_goanna_api_ws).
-record(?STATE, { polling = false,
                  list_all_active_traces_poller_ref }).


init(Req, Opts) ->
    process_flag(trap_exit, true),
    {cowboy_websocket, Req, #?STATE{}}.

websocket_handle({text, ReqJson}, Req, State) ->
    case jsx:decode(ReqJson) of
        [{<<"polling">>,<<"true">>}] ->
            poller(100),
            {reply, {text, <<"ok">>}, Req, State#?STATE{ polling = true }};
        [{<<"polling">>,<<"false">>}] ->
            {reply, {text, <<"ok">>}, Req, State#?STATE{ polling = false }};
        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"pull_all_traces">>},
         {<<"args">>,[]}] ->
            % _SizeInt = list_to_integer(binary_to_list(SizeBin)),
            Json = all_traces_json(),
            {reply, {text, Json}, Req, State};
        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"trace">>},
         {<<"args">>,[Mod,FuncAndAra,TimeSeconds,Messages]}] ->
            ok = trace(Mod,FuncAndAra,TimeSeconds,Messages),
            Json = active_traces_json(),
            {reply, {text, Json}, Req, State};
        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"stop_trace">>},
         {<<"args">>,[]}] ->
            ok = goanna_api:stop_trace(),
            Json = active_traces_json(),
            {reply, {text, Json}, Req,
                State#?STATE{list_all_active_traces_poller_ref = undefined}};
        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"stop_trace">>},
         {<<"args">>,[TrcPattern]}] ->
            [M, F, A] = trcpat_mfa_to_str(TrcPattern),
            %% TODO: handle * as all arity, so call, stop_trace(M,F)
            io:format("~p",[ [M, F, A] ]),
            ok = goanna_api:stop_trace(M, F, A),
            Json = active_traces_json(),
            {reply, {text, Json}, Req, State};
        [{<<"module">>,<<"goanna_api">>},
         {<<"function">>,<<"list_active_traces">>},
         {<<"args">>,[]}] ->
            Json = active_traces_json(),
            {reply, {text, Json}, Req, State};
        UnknownJson ->
            io:format("[~p] UnknownJson: ~p~n", [?MODULE,UnknownJson]),
            Json = jsx:encode([{<<"unknown_json">>, UnknownJson}]),
            {reply, {text, Json}, Req, State}
    end;
websocket_handle(Data, Req, State) ->
    io:format("websocket_handle : ~p\n\n", [Data]),
    {ok, Req, State}.

websocket_info({timeout, _Ref, <<"list_active_traces">>}, Req,
        #?STATE{list_all_active_traces_poller_ref = undefined } = State) ->
    {ok, Req, State};
websocket_info({timeout, _Ref, <<"list_active_traces">>}, Req,
        #?STATE{list_all_active_traces_poller_ref = _ } = State) ->
    case active_traces_json() of
        <<"{\"active_traces\":[]}">> = Json ->
            {reply, {text, Json}, Req, State#?STATE{list_all_active_traces_poller_ref = undefined}};
        Json ->
            R = list_active_traces_poller(),
            {reply, {text, Json}, Req, State#?STATE{list_all_active_traces_poller_ref = R}}
    end;
websocket_info({timeout, _Ref, {<<"fetch">>,_}}, Req, #?STATE{ polling = false } = State) ->
    Json = polling_end_json(),
    {reply, {text, Json}, Req, State};
websocket_info({timeout, _Ref, {<<"fetch">>, Ms}}, Req, #?STATE{ polling = true } = State) ->
    Json =
        case goanna_api:list_active_traces() of
            [] ->
                polling_end_json();
            _Else ->
                poller(Ms),
                all_traces_json()
        end,
    {reply, {text, Json}, Req, State};
websocket_info(Info, Req, State) ->
    io:format("~p~n", [Info]),
    {ok, Req, State}.

poller(Ms) ->
    erlang:start_timer(Ms, self(), {<<"fetch">>, Ms}).

% poller(Ms, Count) when Count =< 0 ->
%     ok;
% poller(Ms, Count) when Count > 0 ->
%     erlang:start_timer(Ms, self(), {<<"fetch">>, Count}).

list_active_traces_poller() ->
    LATPollerRef = erlang:start_timer(1000, self(), <<"list_active_traces">>).

ensure_info(T) ->
    crell_web_utils:ens_bin(io_lib:format("~p", [T])).

ensure_extra(T) ->
    crell_web_utils:ens_bin(io_lib:format("~p", [T])).

dbg_trace_format_to_json({Now, {T, Pid, Label, Info}}) when T==trace ->
    [{<<"datetime">>, crell_web_utils:ens_bin(crell_web_utils:localtime_ms_str(Now))},
     {<<"type">>, crell_web_utils:ens_bin(trace)},
     {<<"pid">>, crell_web_utils:ens_bin(Pid)},
     {<<"label">>, crell_web_utils:ens_bin(Label)},
     {<<"info">>, ensure_info(Info)}];
dbg_trace_format_to_json({Now, {T, Pid, Label, Info, Extra}}) when T==trace ->
    [{<<"datetime">>, crell_web_utils:ens_bin(crell_web_utils:localtime_ms_str(Now))},
     {<<"type">>, crell_web_utils:ens_bin(trace_extra)},
     {<<"pid">>, crell_web_utils:ens_bin(Pid)},
     {<<"label">>, crell_web_utils:ens_bin(Label)},
     {<<"info">>, ensure_info(Info)},
     {<<"extra">>, ensure_extra(Extra)}];
dbg_trace_format_to_json({Now, {T, Pid, Label, Info, _Timestamp}}) when T==trace_ts ->
    [{<<"datetime">>, crell_web_utils:ens_bin(crell_web_utils:localtime_ms_str(Now))},
     {<<"type">>, crell_web_utils:ens_bin(trace)},
     {<<"pid">>, crell_web_utils:ens_bin(Pid)},
     {<<"label">>, crell_web_utils:ens_bin(Label)},
     {<<"info">>, ensure_info(Info)}];
dbg_trace_format_to_json({Now, {T, Pid, Label, Info, Extra, _Timestamp}}) when T==trace_ts ->
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

trace(BinMod,<<"*">>,BinTimeSeconds,BinMessages) ->
    ok = goanna_api:trace_ms(binary_to_list(BinMod)++" -> return ",
        trace_opts(BinTimeSeconds,BinMessages));
trace(BinMod,BinFunc,BinTimeSeconds,BinMessages) ->
    ok = goanna_api:trace_ms(binary_to_list(BinMod)++":"++binary_to_list(BinFunc)++" -> return ",
        trace_opts(BinTimeSeconds,BinMessages)).

trace_opts(<<"">>,<<"">>) ->
    [{time, false}, {messages, false}];
trace_opts(<<"">>, BinMessages) ->
    [{time, false}, {messages, binary_to_integer(BinMessages)}];
trace_opts(BinSecs, <<"">>) ->
    [{time, binary_to_integer(BinSecs)*1000}, {messages, false}];
trace_opts(BinSecs, BinMessages) ->
    [{time, binary_to_integer(BinSecs)*1000}, {messages, binary_to_integer(BinMessages)}].
% trace([{<<"mod">>,Mod},
%        {<<"fun">>,<<"*">>}]) ->
%     goanna_api:trace(crell_web_utils:ens_atom(Mod));
% trace([{<<"mod">>,Mod},
%        {<<"fun">>,Fun}]) ->
%     [F,A] = trcpat_fa_to_str(Fun),
%     goanna_api:trace(crell_web_utils:ens_atom(Mod), F, A).
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
        lists:map(fun({{Node,{trace, [{M,F,A,MatchSpec}]}}, TrcOpts}) ->
            { crell_web_utils:ens_bin(M),
              crell_web_utils:ens_bin(undef_function(F)),
              crell_web_utils:ens_bin(undef_arity(A)),
              crell_web_utils:ens_bin(io_lib:format("~p",[MatchSpec]))
            }
        end, ActiveTracesFull),
    jsx:encode(
        [{<<"active_traces">>,
            [ [ {<<"module">>,   M},
                {<<"function">>, F},
                {<<"arity">>,    A},
                {<<"ms">>,       MS}
              ] || {M,F,A,MS} <- ActiveTraces ]
        }]
    ).

    all_traces_json() ->
        Traces = goanna_api:pull_all_traces(),
        jsx:encode( [{<<"traces">>, [ dbg_trace_format_to_json(T) || T <- Traces ]}] ).

    polling_end_json() ->
        jsx:encode([{<<"traces_polling_end">>, <<"true">>}]).