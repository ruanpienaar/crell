-module (crell_server).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, crell_server_state).
-record(?STATE, { 
    nodes=orddict:new()
}).

-export([
    start_link/0,
    add_node/2,
    remove_node/2,
    nodes/0,
    runtime_modules/1,
    runtime_module_functions/2,
    module_source/2,
    non_sys_processes/1,
    calc_app/2,
    calc_app/3,
    calc_proc/2,
    calc_proc/3,
    calc_app_env/2,
    remote_which_applications/1,
    make_xref/1
]).

-export([inject_module/2,
         purge_module/2
]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

add_node(Node, Cookie) ->
    ConnectedCallBack = [{crell_connect, fun() -> ok=gen_server:cast(?MODULE, {node_connected, Node, Cookie}) end}],
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

nodes() ->
    gen_server:call(?MODULE, nodes).

runtime_modules(Node) ->
    gen_server:call(?MODULE, {runtime_modules, Node}).

runtime_module_functions(Node, Mod) ->
    gen_server:call(?MODULE, {runtime_module_functions, Node, Mod}).

module_source(_Node, Module) ->
    get_source(Module).

non_sys_processes(Node) ->
    gen_server:call(?MODULE, {non_sys_processes, Node}).

calc_app(Node, App) ->
    calc_app(Node,App,[]).

calc_app(Node, App, Opts) ->
    gen_server:call(?MODULE, {app, Node, App, Opts}).

calc_proc(Node, Pid) ->
    calc_proc(Pid, []).

calc_proc(Node, Pid, Opts) ->
    gen_server:call(?MODULE, {proc, Node, Pid, Opts}).

calc_app_env(Node, AppName) ->
   gen_server:call(?MODULE, {app_env, Node, AppName}).

calc_pid(Node, Pid) ->
    gen_server:call(?MODULE, {pid, Node, Pid}).

remote_which_applications(Node) ->
   gen_server:call(?MODULE, {remote_which_applications, Node}).

%% ---------------------------------------

init({}) ->
    process_flag(trap_exit, true),
    {ok, #?STATE{}}.

handle_call(nodes, _From, State) ->
    Nodes = orddict:fetch_keys(State#?STATE.nodes),
    {reply, Nodes, State};
handle_call({runtime_modules, Node}, _From, #?STATE{ nodes = N } = State) ->
    case orddict:find(Node, N) of
        {ok, RemoteState} ->
            UpdatedRemoteModules = rpc:call(Node, crell_remote, get_remote_modules, []),
            Mods = lists:sort(lists:map(fun({Mod, _Exports}) -> Mod end, UpdatedRemoteModules)),
            UpdatedRemoteState = dict:store(remote_modules, UpdatedRemoteModules, RemoteState),
            {reply, {ok, Mods}, State#?STATE{ nodes = orddict:store(Node, UpdatedRemoteState, N) }};
        error ->
            {reply, {error, {unknown_node, Node}}, State}
    end;
    % emote_modules, RM} = lists:keyfind(remote_modules, 1, UpdatedRemoteState),
    % Mods = lists:sort(lists:map(fun({Mod, _Exports}) -> Mod end, RM)),
    % {reply, Mods, State};
handle_call({runtime_module_functions, Node, Mod}, _From, State) ->
    % {remote_modules, RM} = lists:keyfind(remote_modules, 1, State#?STATE.remote_state),
    % case lists:keyfind(Mod, 1, RM) of
    %     false ->
    %         {reply, false, State};
    %     {Mod, Functions} ->
    %         {reply, lists:sort(Functions), State}
    % end;
    {reply, [], State};
handle_call({non_sys_processes, Node}, _From, State) ->
    Reply = 
        case orddict:find(Node, State#?STATE.nodes) of
            {ok, _} ->
                ProcList = rpc:call(Node, crell_remote, non_sys_processes, []),
                {ok, ProcList};
            error ->
                {error, {unknown_node, Node}}
        end,
    {reply, Reply, State};
handle_call({app, Node, App, Opts}, _From, State) ->
    R = rpc:call(Node,crell_remote,calc_app_tree,[App, Opts]),
    {reply, R, State};
handle_call({proc, Node, Pid, Opts}, _From, State) ->
    R = rpc:call(Node,crell_remote,calc_proc_tree,[Pid, Opts]),
    {reply, R, State};
handle_call({app_env, Node, AppName}, _From, State) ->
    %% TODO: find only the required app env from the proplist
    % {remote_all_env, AllAppEnv}
    %     = lists:keyfind(remote_all_env, 1, State#?STATE.remote_state),
    % {AppName, AppEnv}
    %     = lists:keyfind(AppName, 1, AllAppEnv),
    AppEnv = [],
    {reply, AppEnv, State};
handle_call({pid, Node, Pid}, _From, State) ->
    R = rpc:call(Node,crell_remote,calc_proc_tree,[Pid, []]),
    {reply, R, State};
    %% TODO: also build a all app env....
handle_call({remote_which_applications, Node}, _From, State) ->
    RemoteRunningApplications = [], %% lists:keyfind(remote_running_applications, 1, State#?STATE.remote_state)
    {reply, RemoteRunningApplications, State};
handle_call(Request, _From, State) ->
    {reply, {error, unknown_call, ?MODULE, Request}, State}.
%% --------------------------------------------------------------------------

handle_cast({node_connected, Node, Cookie}, State) ->
    {noreply, add_node(Node, Cookie, State)};
handle_cast({node_disconnected, Node, Cookie}, State) ->
    {noreply, remove_node(Node, Cookie, State)};
handle_cast({node_removed, Node, Cookie}, State) ->
    {noreply, remove_node(Node, Cookie, State)};
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
    purge_module(Node, crell_remote),
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

%% This is to follow a function to another....
make_xref(_Node) ->
    ok.