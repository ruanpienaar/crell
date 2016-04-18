-module(crell_trace).
-export([start_link/1,
         name/1
]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-define(SERVER, ?MODULE).
-define(STATE, crell_trace_state).
-record(?STATE, { node }).

name(Node) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Node)).

start_link(Node) ->
    gen_server:start_link({local, name(Node)}, ?MODULE, {Node}, []).

init({Node}) ->
    notify(),
    {ok, #?STATE{ node = Node }}.

handle_call(Request, _From, State) ->
    io:format("Request:~p\n", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast({trace_return, TRC}, State) ->
    io:format("~p\n", [TRC]),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("Msg:~p\n", [Msg]),
    {noreply, State}.

handle_info({nodedown, _Node}, State) ->
    %% TODO: how do i deal with things that dissapear ( nettick time )
    %% TODO: how do i just stop my own tracing locally ?
    notify(),
    {noreply, State};
handle_info(get_traces, #?STATE{ node = Node } = _State) ->
    NewState =
        case
            rpc:call(Node,
                     erlyberly,
                     collect_trace_logs,
                     [])
        of
            {badrpc,nodedown} ->
                notify();
            {ok, []} ->
                get_traces();
            {ok, Traces} ->
                io:format("Traces : ~p\n", [Traces]),
                get_traces()
        end,
    {noreply, NewState};
handle_info(connect, #?STATE{ node = Node } = State) ->
    case net_kernel:connect(Node) of
        true ->
            net_kernel:monitor_nodes(true),
            trace(Node);
        false ->
            notify()
    end,
    {noreply, State};
handle_info(Info, State) ->
    io:format("Info:~p\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

trace(Node) ->
    ok = crell_server:purge_module(Node, erlyberly),
    ok = crell_server:inject_module(erlyberly, Node),
    rpc:call(
        Node,
        erlyberly,
        start_trace,
        [{node(), self()}, application, which_applications, 0, is_exported]
    ),
    get_traces().

notify() ->
    erlang:send_after(1000, self(), connect).

get_traces() ->
    erlang:send_after(100, self(), get_traces).