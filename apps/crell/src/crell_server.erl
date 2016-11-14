-module (crell_server).

-export([
    start_link/0,
    add_node/2,
    remove_node/2
]).



-export([calc_app/1,
         calc_app/2,
         calc_proc/1,
         calc_proc/2,
         calc_app_env/1,
         remote_which_applications/0,
         make_xref/0
]).
-export([
    runtime_modules/0,
    runtime_module_functions/1,
    module_source/1,
    redbug_trace/0,
    redbug_trace/1,
    redbug_trace/2,
    % trace/5,
    redbug_trace_pattern/5,
    get_eb_traces/0,
    get_traces/0,
    clear_traces/0
]).
-export([inject_module/2,
         purge_module/2 ]).

-export([non_sys_processes/0]).

% -export([
%     create_fd/0,
%     listen_accept/0
% ]).
%-export([format_handler/4]).
-export([ remote_trace/0 ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, crell_server_state).
-record(?STATE, { appmon_pid,
                  remote_node,
                  remote_node_cookie,
                  %% TODO: create type for remote state
                  remote_state = [],
                  traces = [],
                  trace_pid,
                  remote_trace_socket,
                  remote_trace_port,
                  remote_modules,
                  nodes=orddict:new()
                }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

add_node(Node, Cookie) ->
    ConnectedCallBack = [{crell_connect, fun() -> ok=gen_server:cast(?MODULE, {node_conencted, Node, Cookie}) end}],
    DisconnCallBack = [{crell_disconnect, fun() -> ok=gen_server:cast(?MODULE, {node_disconnected, Node, Cookie}) end}],
    case hawk:node_exists(Node) of
        {ok, _Pid, Callbacks} ->
            case lists:member(crell_connect, Callbacks) andalso lists:member(crell_disconnect, Callbacks) of
                true ->
                    ok;
                false ->
                    [{NC,CCB}] = ConnectedCallBack,
                    [{ND,DCB}] = DisconnCallBack,
                    CCB(),
                    {ok,updated} = hawk:add_connect_callback(Node, {NC,CCB}),
                    {ok,updated} = hawk:add_disconnect_callback(Node, {ND,DCB}),
                    ok
            end;
        false ->
            {ok,_} = hawk:add_node(Node, Cookie, ConnectedCallBack, DisconnCallBack),
            ok
    end.

remove_node(Node, Cookie) ->
    case hawk:node_exists(Node) of
        {ok, _Pid, _Callbacks} ->
            ok = hawk:remove_node(Node),
            ok = gen_server:cast(?MODULE, {node_removed, Node, Cookie});
        false ->
            ok
    end.





%% Refactor all the below ones.
%% TODO: add node to all the below calls.

calc_app(App) ->
    calc_app(App,[]).

calc_app(App,Opts) ->
    gen_server:call(?MODULE, {app,App,Opts}).

calc_proc(Pid) ->
    calc_proc(Pid,[]).

calc_proc(Pid,Opts) ->
    gen_server:call(?MODULE, {pid,Pid,Opts}).

calc_app_env(AppName) ->
   gen_server:call(?MODULE, {app_env,AppName}).

calc_pid(Pid) ->
    gen_server:call(?MODULE, {pid, Pid}).

remote_which_applications() ->
   gen_server:call(?MODULE, remote_which_applications).

runtime_modules() ->
    gen_server:call(?MODULE, runtime_modules).

runtime_module_functions(Mod) ->
    gen_server:call(?MODULE, {runtime_module_functions, Mod}).

%% TODO: make a function that'll create the empty values...

%% all_functions, all_arities
redbug_trace() ->
    {ok,_} =
        redbug_trace([ {crell_remote, test_function, 0, "", return_stack},
                       {crell_remote, another_function, 0, "", return_stack}
              ]),
    crell_remote:test_function(),
    crell_remote:another_function(),
    crell_remote:test_function(),
    crell_remote:another_function(),
    crell_remote:another_function().

%% TODO: Add type [ {m, f, a, g, r, opts}, ... ].
redbug_trace(Specs) when is_list(Specs) ->
    redbug_trace(Specs, []);
redbug_trace(M) when is_atom(M) ->
    redbug_trace({M, all_functions, all_arities, "", return, []});
redbug_trace({M, F, A, G, R, Opts})
        % when
        % (is_atom(M) and is_atom(F) and (is_list(A) or all_arities) and is_list(G))
        % andalso
        % ((R == return) or (R == stack) or (R == return_stack))
        ->
    gen_server:call(?MODULE, {redbug_trace, M, F, A, G, R, Opts}).

redbug_trace(Specs, Opts) when is_list(Specs), is_list(Opts) ->
    gen_server:call(?MODULE, {redbug_trace, Specs, Opts});
redbug_trace(M, F) when is_atom(M), is_atom(F) ->
    redbug_trace({M, F, all_arities, "", return_stack, []}).

remote_trace() ->
    gen_server:call(?MODULE,
                    {rtrace, application, which_applications, 0}).

get_eb_traces() ->
    gen_server:call(?MODULE, {get_eb_traces}).

get_traces() ->
    gen_server:call(?MODULE, {get_traces}).

clear_traces() ->
    gen_server:call(?MODULE, {clear_traces}).

%% ---------------------------------------

non_sys_processes() ->
    gen_server:call(?MODULE, {non_sys_processes}).

%% ---------------------------------------

init({}) ->
    process_flag(trap_exit, true),
    {ok, #?STATE{}}.

handle_call({app,App,Opts}, _From, #?STATE{ remote_node = Node } = State) ->
    R = rpc:call(Node,crell_remote,calc_app_tree,[App, Opts]),
    {reply, R, State};
handle_call({pid,Pid,Opts}, _From, #?STATE{ remote_node = Node } = State) ->
    R = rpc:call(Node,crell_remote,calc_proc_tree,[Pid, Opts]),
    {reply, R, State};
handle_call({app_env,AppName}, _From, #?STATE{ remote_node = _Node } = State) ->
    %% TODO: find only the required app env from the proplist
    {remote_all_env, AllAppEnv}
        = lists:keyfind(remote_all_env, 1, State#?STATE.remote_state),
    {AppName, AppEnv}
        = lists:keyfind(AppName, 1, AllAppEnv),
    {reply,AppEnv,State};
handle_call({pid, Pid}, _From, #?STATE{ remote_node = Node } = State) ->
    R = rpc:call(Node,crell_remote,calc_proc_tree,[Pid, []]),
    {reply, R, State};
    %% TODO: also build a all app env....
handle_call(remote_which_applications, _From, State) ->
   {reply, lists:keyfind(remote_running_applications, 1, State#?STATE.remote_state),State};
handle_call(runtime_modules, _From, #?STATE{remote_node=Node} = State) ->
    {ok, UpdatedRemoteState} = rpc:call(Node, crell_remote, state, []),
    {remote_modules, RM} = lists:keyfind(remote_modules, 1, UpdatedRemoteState),
    Mods = lists:sort(lists:map(fun({Mod, _Exports}) -> Mod end, RM)),
    {reply, Mods, State#?STATE{ remote_state = UpdatedRemoteState }};
handle_call({runtime_module_functions, Mod}, _From, State) ->
    {remote_modules, RM} = lists:keyfind(remote_modules, 1, State#?STATE.remote_state),
    case lists:keyfind(Mod, 1, RM) of
        false ->
            {reply, false, State};
        {Mod, Functions} ->
            {reply, lists:sort(Functions), State}
    end;
handle_call({redbug_trace, Specs, Opts1}, _From, #?STATE{ remote_node = Node } = State) ->
    Trcs = [ redbug_trace_pattern(M, F, A, G, R) || {M,F,A,G,R} <- Specs ],
    Pid = do_trace(Trcs, Opts1, Node),
    {reply, {ok, Pid}, State#?STATE{ trace_pid = Pid }};
handle_call({redbug_trace, M, F, A, G, R, Opts1}, _From,
            #?STATE{ remote_node = Node } = State) ->
    Trc = redbug_trace_pattern(M, F, A, G, R),
    Pid = do_trace(Trc, Opts1, Node),
    {reply, {ok, Pid}, State#?STATE{ trace_pid = Pid }};
handle_call({rtrace, M, F, A}, _From,
            #?STATE{ remote_node = Node } = State) ->
    Port = random_avail_port(),
    Host = "localhost",
    {ok, LSock} = gen_tcp:listen(Port, [binary]),
    ok = rpc:call(Node, crell_remote, setup_traceing, [Host, Port]),
    {ok, Socket} = gen_tcp:accept(LSock),
    ok = inet:setopts(Socket, [{active, once}]),
    ok = gen_tcp:controlling_process(Socket, self()),
    ok = rpc:call(Node, crell_remote, trace, [M,F,A]),
    {reply, ok, State#?STATE{ remote_trace_socket = Socket,
                              remote_trace_port = Port }};
handle_call({get_eb_traces}, _From, State) ->
    {reply, {ok, {whereis(redbug),lists:reverse(State#?STATE.traces)}}, State};
handle_call({clear_traces}, _From, State) ->
    {reply, {ok, whereis(redbug)}, State#?STATE{ traces = []}};
handle_call({non_sys_processes}, _From, #?STATE{ remote_node = Node } = State) ->
    ProcList = rpc:call(Node, crell_remote, non_sys_processes, []),
    {reply, {ok, ProcList}, State};
handle_call(Request, _From, State) ->
    {reply, {error, unknown_call, ?MODULE, Request}, State}.

%% --------------------------------------------------------------------------

handle_cast({node_conencted, Node, Cookie}, State) ->
    {noreply, add_node(Node, Cookie, State)};
handle_cast({node_disconnected, Node, Cookie}, State) ->
    {noreply, remove_node(Node, Cookie, State)};
handle_cast({node_removed, Node, Cookie}, State) ->
    {noreply, remove_node(Node, Cookie, State)};
handle_cast({handle_trace, T}, State) ->
    {noreply, State#?STATE{ traces = [T|State#?STATE.traces] }};
handle_cast(_Msg, State) ->
    {noreply, State}.

add_node(Node, Cookie, #?STATE{ nodes = N } = State) ->
    {ok, RemoteState} = start_remote_code(Node, Cookie),
    State#?STATE{ nodes = orddict:store(Node, RemoteState, N) }.

remove_node(Node, _Cookie, #?STATE{ nodes = N } = State) ->
    State#?STATE{ nodes = orddict:erase(Node, N) }.

start_remote_code(Node,Cookie) ->
    case application:get_env(crell, remote_action) of
        {ok, 1} ->
            % try loading module
            case inject_module(crell_remote, Node) of
                ok ->
                    {ok, _RemoteState} = rpc:call(Node, crell_remote, init, []);
                _ ->
                    {stop, enotloaded}
            end;
        {ok, 2} ->
            % TODO: try lib, and call remote state
            %% rpc:call(Node, crell_remote, init, []),
            {ok, todo_get_remote_state};
        {ok, 3} ->
            % TODO: try loading, then lib,
            %% case inject_module(crell_remote,Node) of
            %%% TODO: complete it
            {ok, todo_get_remote_state};
        undefined ->
            {ok, todo_get_remote_state}
    end.


-spec inject_module(ModName :: term(), NodeName :: atom()) ->
                                        ok | {error, Reason :: term()}.
inject_module(ModName, NodeName) ->
    lager:info("Inject the agent code into the node (NodeName=~p, "
               "Agent=~p)", [NodeName, ModName]),
    case code:get_object_code(ModName) of
        {ModName, Bin, File} ->
            case rpc:call(NodeName, code, load_binary,
                          [ModName, File, Bin]) of
                {module, ModName} ->
                    purge_module(NodeName, ModName), % remove old code of module code
                    lager:info("Agent injected (NodeName=~p, Agent=~p)",
                               [NodeName, ModName]),
                    ok;
                {Error, Reason} when Error =:= error;
                                     Error =:= badrpc ->
                    lager:error("rpc(~p, code, load_binary, ...) "
                                "failed (ModName=~p, Reason=~p)",
                                [NodeName, ModName, Reason]),
                    {error, {load_binary_failed, Reason}}
            end;
        error ->
            lager:error("code:get_object_code failed (ModName=~p)",
                        [ModName]),
            {error, {get_object_code_failed, ModName}}
    end.

  purge_module(Node, Module) ->
    Res = try rpc:call(Node, code, soft_purge, [Module]) of
              true ->
                  ok;
              false ->
                  hard_purge_module(Node, Module);
              {badrpc, _} = RPCError ->
                  {error, RPCError}
          catch
              C:E ->
                  {error, {C,E}}
          end,
    case Res of
        ok ->
            lager:info("Purged ~p from ~p", [Module, Node]);
        {error, Error} ->
            lager:error("Error while purging  ~p from ~p: ~p", [Module, Node, Error])
    end,
    ok.

hard_purge_module(Node, Module) ->
    try rpc:call(Node, code, purge, [Module]) of
        true ->
            lager:info("Purging killed processes on ~p while loading ~p", [Node, Module]),
            ok;
        false ->
            ok;
        {badrpc, _} = RPCError ->
            {error, RPCError}
    catch
        C:E ->
            {error, {C,E}}
    end.

%% --------------------------------------------------------------------------

handle_info({tcp, Socket, Data},
        #?STATE{ remote_trace_socket = Socket } = State) ->
    ok = inet:setopts(Socket,[{active, once}]),
    do_handle_tcp(Data),
    {noreply, State};
handle_info({tcp_closed,_Socket},State) ->
    reopen_connection(),
    {noreply, State};
handle_info({tcp_error, _Socket, _Reason},
        #?STATE{ remote_trace_socket = Socket } = State) ->
    ok = gen_tcp:close(Socket),
    {noreply, State#?STATE{ remote_trace_socket = undefined,
                            remote_trace_port = undefined}};
handle_info({'EXIT', Pid, Reason}, State) when Pid == State#?STATE.trace_pid ->
    io:format("trace handle_info ~p\n\n", [{'EXIT', Pid, Reason}]),
    {noreply, State#?STATE{ trace_pid = undefined }};
handle_info({'EXIT', Pid, Reason}, State) when Pid == State#?STATE.trace_pid ->
    io:format("handle_info ~p\n\n", [{'EXIT', Pid, Reason}]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("handle_info ~p\n\n", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------------


%% This is to follow a function to another....
make_xref() ->
    ok.

% module_functions(Mod) ->
%     Exports = Mod:module_info(exports),
%     Unexported = [F || F <- Mod:module_info(functions), not lists:member(F, Exports)],
%     {Mod, Exports, Unexported}.

module_source(Module) ->
  get_source(Module).

-spec abstract_code(module()) -> [erl_parse:abstract_form()].
abstract_code(Module) ->
        File = code:which(Module),
        {ok,{_Mod,[{abstract_code,{_Version,Forms}}]}} = beam_lib:chunks(File, [abstract_code]),
        Forms.

mod_src(Module) ->
    Forms = abstract_code(Module),
    lists:flatten([[erl_pp:form(F),$\n] || F <- Forms, element(1,F) =:= attribute orelse element(1,F) =:= function]).

fun_src(Mod, Fun, Arity) ->
    Forms = abstract_code(Mod),
    [FF] = [FF || FF = {function, _Line, Fun2, Arity2, _} <- Forms, Fun2 =:= Fun, Arity2 =:= Arity],
    lists:flatten(erl_pp:form(FF)).

-spec get_source(module() | {module(), function(), Arity :: non_neg_integer()}) -> {ok, string()} | {error, Reason :: any()}.
get_source(What) ->
        try
        case What of
                {M,F,A} ->
                        {ok, fun_src(M,F,A)};
                Mod when is_atom(Mod)->
                        {ok, mod_src(Mod)}
        end
        catch _:E ->
                {error, E}
        end.


create_fd() ->
    spawn(?MODULE, listen_accept, []).

% connect_fd() ->
%     ok.

listen_accept() ->
    {ok, ListenSocket} =
        gen_tcp:listen(
            8765,
            [binary, {packet, raw}, {active, false}, {reuseaddr, true}]
        ),
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        {ok,ConnCtrlPID} = crell_tcp_controller:start_link(Socket),
        gen_tcp:controlling_process(Socket, ConnCtrlPID).

format_handler(A, B, C, D) ->
    io:format("~p\n", [A]),
    io:format("~p\n", [B]),
    io:format("~p\n", [C]),
    io:format("~p\n", [D]),
    ok.

% ets:new(ttb_history_table,[ordered_set,named_table,public]).
% ttb:start_trace([node()],
% [{crell_remote, module_info, []}],
% {all, call},
% [
%     shell,
%     {handler,{fun crell_server:format_handler/4, _InitState=[]}}
% ]).

% M all_functions, all_arities
% M F              all_arities
% M F A



% "mod"
redbug_trace_pattern(M, all_functions, all_arities, _G, R) ->
    atom_to_list(M)
    ++ " -> " ++ return_stack_trace_patter(R);

% "mod:fun"
redbug_trace_pattern(M, F, all_arities, _G, R) ->
    atom_to_list(M) ++ ":"
    ++ atom_to_list(F)
    ++ " -> " ++ return_stack_trace_patter(R);

% "mod:fun/3"
redbug_trace_pattern(M, F, A, _G, R) when is_integer(A) ->
    atom_to_list(M) ++ ":"
    ++ atom_to_list(F) ++ "/"
    ++ integer_to_list(A)
    ++ " -> " ++ return_stack_trace_patter(R);

% "mod:fun('_',atom,X)"
redbug_trace_pattern(M, F, A, G, R) when is_list(A) ->
    atom_to_list(M) ++ ":"
    ++ atom_to_list(F)
    ++ arglist_to_trace_pattern(A, G)
    ++ " -> " ++ return_stack_trace_patter(R).


return_stack_trace_patter(return) ->
    "return";
return_stack_trace_patter(stack) ->
    "stack";
return_stack_trace_patter(return_stack) ->
    return_stack_trace_patter(return)
    ++ ";"
    ++ return_stack_trace_patter(stack).

arglist_to_trace_pattern(Args, Guard) ->
    arglist_to_trace_pattern(Args, Guard, []).

%% TODO: improve the string handling, why do we need flattening...
arglist_to_trace_pattern([], "", R) ->
    lists:flatten("(" ++ lists:reverse(R) ++ ") ");
arglist_to_trace_pattern([], Guard, R) ->
    A = lists:flatten("(" ++ lists:reverse(R) ++ ") when " ++ Guard ++ " "),
    io:format("~p\n", [A]),
    A;
arglist_to_trace_pattern([Arg|RestArgs=[]], Guard, R) when is_list(Arg) ->
    arglist_to_trace_pattern(RestArgs, Guard, [io_lib:format("~s", [Arg])|R]);
arglist_to_trace_pattern([Arg|RestArgs=[]], Guard, R) ->
    arglist_to_trace_pattern(RestArgs, Guard, [io_lib:format("~p", [Arg])|R]);
arglist_to_trace_pattern([Arg|RestArgs], Guard, R) when is_list(Arg) ->
    arglist_to_trace_pattern(RestArgs, Guard, [io_lib:format("~s,", [Arg])|R]);
arglist_to_trace_pattern([Arg|RestArgs], Guard, R) ->
    arglist_to_trace_pattern(RestArgs, Guard, [io_lib:format("~p,", [Arg])|R]).


% Case starts with upper case, and an item in
% char_to_string(X) ->

do_trace(Trc, Opts1, Node) ->
    io:format("Trc : ~p\n", [Trc]),
    Opts = [{target, Node},
            {print_fun, fun(T) -> gen_server:cast(?MODULE, {handle_trace, T}) end},
            {time, 50000},
            {msgs, 10000},
            {blocking, true}
          ] ++ Opts1,
    spawn_link(fun() ->
        case redbug:start(Trc,Opts) of
            redbug_already_started ->
                redbug_already_started;
            {oops, {C, R}} ->
                {oops, {C, R}};
            {Procs, Funcs} ->
                {Procs, Funcs}
        end
    end).

random_avail_port() ->
    33333.

do_handle_tcp(TcpData) ->
    io:format("TcpData : ~p\n", [binary_to_term(TcpData)]).

reopen_connection() ->
    ok.