-module (crell_server).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, crell_server_state).
-record(?STATE, {
    nodes=orddict:new(),
    tracing=false
}).

-export([
    start_link/0,
    add_node/2,
    remove_node/1,
    nodes/0,
    runtime_modules/1,
    runtime_module_functions/2,
    %module_source/2,
    non_sys_processes/1,
    calc_app/2,
    calc_app/3,
    calc_proc/2,
    calc_proc/3,
    calc_app_env/2,
    remote_which_applications/1,
    make_xref/1
]).

-export([
    is_tracing/0,
    toggle_tracing/0
]).

-export([inject_module/2,
         purge_module/2
]).

%% TODO: choose a better data structure, or use ets...
%% The orddict, is not coping with the amounts of data from the module function exports...

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


%% TODO: extremely slow adding nodes, block other nodes from being added...FIXME
add_node(Node, Cookie) ->
    ConnectedCallBack = [{crell_connect, fun() ->
        ok=gen_server:call(?MODULE, {node_connected, Node, Cookie}, infinity) end}],
    DisconnCallBack = [{crell_disconnect, fun() ->
        ok=gen_server:call(?MODULE, {node_disconnected, Node, Cookie}, infinity) end}],
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

remove_node(Node) ->
    ok = purge_module(Node, crell_remote),
    ok = hawk:remove_node(Node).

nodes() ->
    gen_server:call(?MODULE, nodes, infinity).

%% Maybe we should bundle modules per application ?

runtime_modules(Node) ->
    gen_server:call(?MODULE, {runtime_modules, Node}).

runtime_module_functions(Node, Mod) ->
    gen_server:call(?MODULE, {runtime_module_functions, Node, Mod}).

% module_source(_Node, Module) ->
%     get_source(Module).

non_sys_processes(Node) ->
    gen_server:call(?MODULE, {non_sys_processes, Node}).

calc_app(Node, App) ->
    calc_app(Node,App,[]).

calc_app(Node, App, Opts) ->
    gen_server:call(?MODULE, {app, Node, App, Opts}).

calc_proc(Node, Pid) ->
    calc_proc(Node, Pid, []).

calc_proc(Node, Pid, Opts) ->
    gen_server:call(?MODULE, {proc, Node, Pid, Opts}).

calc_app_env(Node, AppName) ->
   gen_server:call(?MODULE, {app_env, Node, AppName}).

remote_which_applications(Node) ->
   gen_server:call(?MODULE, {remote_which_applications, Node}).

%% This is to follow a function to another....
make_xref(_Node) ->
    ok.

is_tracing() ->
    gen_server:call(?MODULE, is_tracing).

toggle_tracing() ->
    gen_server:call(?MODULE, toggle_tracing).

%% ---------------------------------------

init({}) ->
    process_flag(trap_exit, true),
    {ok, #?STATE{}}.

node_ordict_values(Node, #?STATE{ nodes = Nodes } = State, LookupValue) ->
    case orddict:find(Node, Nodes) of
        {ok, RemoteState} ->
            {ok, Response, UpdatedRemoteState} = do_get_values(Node, RemoteState, LookupValue),
            UpdatedState = State#?STATE{ nodes = orddict:store(Node, UpdatedRemoteState, Nodes) },
            {ok, Response, UpdatedState};
        error ->
            {ok, {error, {unknown_node, Node}}, State}
    end.

%% TODO: rather make explicit remote update calls, update_remote_modules
do_get_values(_Node, RemoteState, get_remote_modules) ->
    RemoteModules = dict:fetch(remote_modules, RemoteState),
    Mods = lists:sort(lists:map(fun({Mod, _Exports}) -> Mod end, RemoteModules)),
    {ok, Mods, RemoteState};
do_get_values(_Node, RemoteState, {get_remote_module_functions, Mod}) ->
    RemoteModules = dict:fetch(remote_modules, RemoteState),
    case lists:keyfind(Mod, 1, RemoteModules) of
        false ->
            %% Maybe update when not found...
            {ok, false, RemoteState};
        {Mod, Functions} ->
            {ok, lists:sort(Functions), RemoteState}
    end;
do_get_values(Node, RemoteState, non_sys_processes) ->
    ProcList = rpc:call(Node, crell_remote, non_sys_processes, []),
    {ok, ProcList, RemoteState};
do_get_values(Node, RemoteState, {app_env, AppName}) ->
    RemoteState2 = rpc:call(Node, crell_remote, remote_all_env, [RemoteState]),
    RemoteApps = dict:fetch(remote_all_env, RemoteState2),
    case lists:keyfind(AppName, 1, RemoteApps) of
        false ->
            {ok, false, RemoteState2};
        {AppName, AppEnv} ->
            {ok, AppEnv, RemoteState2}
    end;
do_get_values(Node, RemoteState, remote_which_applications) ->
    RemoteState2 = rpc:call(Node, crell_remote, remote_applications, [RemoteState]),
    RemoteApps = dict:fetch(remote_running_applications, RemoteState2),
    {ok, RemoteApps, RemoteState2}.

handle_call(nodes, _From, State) ->
    Nodes = orddict:fetch_keys(State#?STATE.nodes),
    {reply, Nodes, State};
handle_call({runtime_modules, Node}, _From, State) ->
    % Update state here
    {ok, Reply, NewState} = node_ordict_values(Node, State, get_remote_modules),
    {reply, Reply, NewState};
handle_call({runtime_module_functions, Node, Mod}, _From, State) ->
    % Update state here
    {ok, Reply, _} = node_ordict_values(Node, State, {get_remote_module_functions, Mod}),
    {reply, Reply, State};
handle_call({non_sys_processes, Node}, _From, State) ->
    % Update state here
    {ok, Reply, _} = node_ordict_values(Node, State, non_sys_processes),
    {reply, Reply, State};
handle_call({app, Node, App, Opts}, _From, State) ->
    % Update state here
    R = rpc:call(Node,crell_remote,calc_app_tree,[App, Opts]),
    {reply, R, State};
handle_call({app_env, Node, AppName}, _From, State) ->
    % Update state here
    {ok, Reply, NewState} = node_ordict_values(Node, State, {app_env, AppName}),
    {reply, Reply, NewState};
handle_call({pid, Node, Pid}, _From, State) ->
    % Update state here
    R = rpc:call(Node,crell_remote,calc_proc_tree,[Pid, []]),
    {reply, R, State};
    %% TODO: also build a all app env....
handle_call({remote_which_applications, Node}, _From, State) ->
    % Update state here
    % {ok, RemoteState} = start_remote_code(Node, Cookie),
    % State#?STATE{ nodes = orddict:store(Node, RemoteState, N) }.
    %{ok, RemoteState} = remote_state(),
    {ok, Reply, _} = node_ordict_values(Node, State, remote_which_applications),
    {reply, Reply, State};
handle_call(E={node_connected, Node, Cookie}, _From, State) ->
    crell_notify:action({node_connecting, Node}),
    NewState = add_node(Node, Cookie, State),
    crell_notify:action(E),
    {reply, ok, NewState};
handle_call(E={node_disconnected, Node, Cookie}, _From, State) ->
    crell_notify:action(E),
    {reply, ok, remove_node(Node, Cookie, State)};

handle_call(is_tracing, _From, State) ->
    {reply, State#?STATE.tracing, State};
%% TODO: maybe add some more conditions,
handle_call(toggle_tracing, _From, #?STATE{tracing=true} = State) ->
    {reply, false, State#?STATE{ tracing = false }};
handle_call(toggle_tracing, _From, #?STATE{tracing=false} = State) ->
    {reply, true, State#?STATE{ tracing = true }};

handle_call(Request, _From, State) ->
    {reply, {error, unknown_call, ?MODULE, Request}, State}.
%% --------------------------------------------------------------------------
% handle_cast({node_connected, Node, Cookie}, State) ->
%     {noreply, add_node(Node, Cookie, State)};
% handle_cast({node_disconnected, Node, Cookie}, State) ->
%     {noreply, remove_node(Node, Cookie, State)};
% handle_cast({node_removed, Node, Cookie}, State) ->
%     {noreply, remove_node(Node, Cookie, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------------

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

add_node(Node, Cookie, #?STATE{ nodes = N } = State) ->
    {ok, RemoteState} = start_remote_code(Node, Cookie),
    State#?STATE{ nodes = orddict:store(Node, RemoteState, N) }.

remove_node(Node, _Cookie, #?STATE{ nodes = N } = State) ->
    State#?STATE{ nodes = orddict:erase(Node, N) }.

start_remote_code(Node,_Cookie) ->
    case application:get_env(crell, remote_action) of
        {ok, 1} ->
            % try loading module
            case inject_module(crell_remote, Node) of
                ok -> {ok, _RemoteState} = rpc:call(Node, crell_remote, init, []);
                _  -> {stop, enotloaded}
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
    io:format("Inject the agent code into the node (NodeName=~p, "
               "Agent=~p)~n", [NodeName, ModName]),
    case code:get_object_code(ModName) of
        {ModName, Bin, File} ->
            case rpc:call(NodeName, code, load_binary,
                          [ModName, File, Bin]) of
                {module, ModName} ->
                    purge_module(NodeName, ModName), % remove old code of module code
                    io:format("Agent injected (NodeName=~p, Agent=~p)~n",
                               [NodeName, ModName]),
                    ok;
                {Error, Reason} when Error =:= error;
                                     Error =:= badrpc ->
                    io:format("rpc(~p, code, load_binary, ...) "
                                "failed (ModName=~p, Reason=~p)~n",
                                [NodeName, ModName, Reason]),
                    {error, {load_binary_failed, Reason}}
            end;
        error ->
            io:format("code:get_object_code failed (ModName=~p)~n",
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
            io:format("Purged ~p from ~p~n", [Module, Node]);
        {error, Error} ->
            io:format("Error while purging  ~p from ~p: ~p~n", [Module, Node, Error])
    end,
    ok.

hard_purge_module(Node, Module) ->
    try rpc:call(Node, code, purge, [Module]) of
        true ->
            io:format("Purging killed processes on ~p while loading ~p~n", [Node, Module]),
            ok;
        false ->
            io:format("Could not code:purge ~p~n", [Module]);
        {badrpc, _} = RPCError ->
            {error, RPCError}
    catch
        C:E ->
            {error, {C,E}}
    end.

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

% -spec get_source(module() | {module(), function(), Arity :: non_neg_integer()}) -> {ok, string()} | {error, Reason :: any()}.
% get_source(What) ->
%         try
%         case What of
%                 {M,F,A} ->
%                         {ok, fun_src(M,F,A)};
%                 Mod when is_atom(Mod)->
%                         {ok, mod_src(Mod)}
%         end
%         catch _:E ->
%                 {error, E}
%         end.