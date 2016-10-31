-module(crell_goanna_api_ws).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
    erlang:register(crell_goanna_api_ws, self()),
    process_flag(trap_exit, true),
    % io:format("~p~n~p~n", [self(),erlang:process_info(self())]),
    % OUTPUT:
    %     <0.1640.0>
    % [{registered_name,crell_goanna_api_ws},
    %  {current_function,{crell_goanna_api_ws,init,2}},
    %  {initial_call,{cowboy_protocol,init,4}},
    %  {status,running},
    %  {message_queue_len,0},
    %  {messages,[]},
    %  {links,[<0.1444.0>,#Port<0.3379>]},
    %  {dictionary,[]},
    %  {trap_exit,true},
    %  {error_handler,error_handler},
    %  {priority,normal},
    %  {group_leader,<0.1350.0>},
    %  {total_heap_size,1635},
    %  {heap_size,610},
    %  {stack_size,16},
    %  {reductions,1427},
    %  {garbage_collection,[{max_heap_size,#{error_logger => true,kill => true,size => 0}},
    %                       {min_bin_vheap_size,46422},
    %                       {min_heap_size,233},
    %                       {fullsweep_after,65535},
    %                       {minor_gcs,6}]},
    %  {suspending,[]}]
    % erlang:start_timer(1000, self(), <<"Hello!">>),
    {cowboy_websocket, Req, Opts}.

websocket_handle({text, <<"fetch">>}, Req, State) ->
    % fetcher(25),
    {reply, {text, <<"ok">>}, Req, State};
% websocket_handle({text, Msg}, Req, State) ->
%     {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

% {text,<<"{\"fetch\":\"50\"}">>}
websocket_handle({text, ReqJson}, Req, State) ->
    case jsx:decode(ReqJson) of
        [{<<"trace">>, Details}] ->
            ok = trace(Details),
            {reply, {text, <<"ok">>}, Req, State};
        [{<<"fetch">>,_SizeBin}] ->
            % _SizeInt = list_to_integer(binary_to_list(SizeBin)),
            Traces = goanna_api:pull_all_traces(),
            Json = jsx:encode( [ dbg_trace_format_to_json(T) || T <- Traces ] ),
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
       {<<"fun">>,<<>>},
       {<<"ara">>,<<>>},
       {<<"tim">>,_Time},
       {<<"mes">>,_Messages}]) ->
    goanna_api:trace(crell_web_utils:ens_atom(Mod));
trace([{<<"mod">>,Mod},
       {<<"fun">>,Fun},
       {<<"ara">>,<<>>},
       {<<"tim">>,_Time},
       {<<"mes">>,_Messages}]) ->
    goanna_api:trace(crell_web_utils:ens_atom(Mod), crell_web_utils:ens_atom(Fun));
trace([{<<"mod">>,Mod},
       {<<"fun">>,Fun},
       {<<"ara">>,Arity},
       {<<"tim">>,_Time},
       {<<"mes">>,_Messages}]) ->
    goanna_api:trace(crell_web_utils:ens_atom(Mod), crell_web_utils:ens_atom(Fun), crell_web_utils:ens_int(Arity)).




