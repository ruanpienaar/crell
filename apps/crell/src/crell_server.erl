-module (crell_server).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, crell_server_state).
-record(?STATE, {
    nodes=orddict:new(),
    tracing=false,
    cluster_modules
}).

-export([
    start_link/0,
    add_node/2,
    remove_node/1,
    nodes/0,
    runtime_modules/1,
    cluster_modules/0,
    cluster_module_functions/1,
    % runtime_module_functions/2,
    %module_source/2,
    non_sys_processes/1,
    calc_app/2,
    calc_app/3,
    calc_proc/2,
    calc_proc/3,
    calc_app_env/2,
    remote_which_applications/1,
    make_xref/1,
    get_db_tables/1,
    dump_ets_tables/2,
    dump_mnesia_tables/2
]).

-export([
    is_tracing/0,
    toggle_tracing/0
]).

-export([inject_module/2,
         purge_module/2
]).

-export([
    cluster_application_consistency/0
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

cluster_modules() ->
    gen_server:call(?MODULE, cluster_modules).

cluster_module_functions(Mod) ->
    gen_server:call(?MODULE, {cluster_module_functions, Mod}).

% runtime_module_functions(Node, Mod) ->
%     gen_server:call(?MODULE, {runtime_module_functions, Node, Mod}).

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

get_db_tables(Node) ->
    gen_server:call(?MODULE, {get_db_tables, Node}).

dump_ets_tables(Node, Tables) ->
    gen_server:call(?MODULE, {dump_ets_tables, Node, Tables}).

dump_mnesia_tables(Node, Tables) ->
    gen_server:call(?MODULE, {dump_mnesia_tables, Node, Tables}).

is_tracing() ->
    gen_server:call(?MODULE, is_tracing).

toggle_tracing() ->
    gen_server:call(?MODULE, toggle_tracing).

cluster_application_consistency() ->
    gen_server:call(?MODULE, cluster_application_consistency).

%% ---------------------------------------

init({}) ->
    %% TODO: maybe check with Hawk, who's already connected...
    process_flag(trap_exit, true),
    {ok, #?STATE{}}.

update_state(Node, #?STATE{ nodes = Nodes } = State, LookupValue) ->
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
    {reply, state_nodes(State), State};
handle_call({runtime_modules, Node}, _From, State) ->
    % Update state here
    {ok, Reply, NewState} = update_state(Node, State, get_remote_modules),
    {reply, Reply, NewState};


handle_call(cluster_modules, _From, State) ->
    %% TODO: we need to make a update cluster_state function
    %% TODO: group erlang nodes together based on a cookie/some-family, some label??
    %% Aggregate the nodes' modules, check consistency, and update state.

    %% TODO: hack, just use the last Node's modules...
    LastNode = lists:last(orddict:fetch_keys(State#?STATE.nodes)),
    {ok, Reply, NewState} = update_state(LastNode, State, get_remote_modules),

    {reply, Reply, NewState};
handle_call({cluster_module_functions,Mod}, _From, State) ->
    %% TODO: hack, just use the last Node's modules...
    LastNode = lists:last(orddict:fetch_keys(State#?STATE.nodes)),
    {ok, Reply, NewState} = update_state(LastNode, State, {get_remote_module_functions,Mod}),
    {reply, Reply, NewState};
% handle_call({runtime_module_functions, Node, Mod}, _From, State) ->
%     % Update state here
%     {ok, Reply, _} = update_state(Node, State, {get_remote_module_functions, Mod}),
%     {reply, Reply, State};

handle_call({non_sys_processes, Node}, _From, State) ->
    % Update state here
    {ok, Reply, _} = update_state(Node, State, non_sys_processes),
    {reply, Reply, State};
handle_call({app, Node, App, Opts}, _From, State) ->
    % Update state here
    R = rpc:call(Node,crell_remote,calc_app_tree,[App, Opts]),
    {reply, R, State};
handle_call({app_env, Node, AppName}, _From, State) ->
    % Update state here
    {ok, Reply, NewState} = update_state(Node, State, {app_env, AppName}),
    {reply, Reply, NewState};
handle_call({pid, Node, Pid}, _From, State) ->
    % Update state here
    R = rpc:call(Node,crell_remote,calc_proc_tree,[Pid, []]),
    {reply, R, State};
    %% TODO: also build a all app env....
handle_call({remote_which_applications, Node}, _From, State) ->
    {ok, Reply, _} = update_state(Node, State, remote_which_applications),
    {reply, Reply, State};
handle_call(E={node_connected, Node, Cookie}, _From, State) ->
    crell_notify:action({node_connecting, Node}),
    NewState = add_node(Node, Cookie, State),
    crell_notify:action(E),
    {reply, ok, NewState};
handle_call(E={node_disconnected, Node, Cookie}, _From, State) ->
    NewState = do_remove_node(E, Node, Cookie, State),
    {reply, ok, NewState};
handle_call(is_tracing, _From, State) ->
    {reply, State#?STATE.tracing, State};
%% TODO: maybe add some more conditions,
handle_call(toggle_tracing, _From, #?STATE{tracing=true} = State) ->
    ok = orddict:fold(fun(Node,NodeDict,ok) ->
        % io:format("Disable tracing on : ~p~n", [Node]),
        ok = goanna_api:stop_trace(),
        true = goanna_api:remove_goanna_callbacks(Node),
        ok = goanna_api:remove_goanna_node(Node)
    end, ok, State#?STATE.nodes),
    {reply, false, State#?STATE{ tracing = false }};
handle_call(toggle_tracing, _From, #?STATE{tracing=false} = State) ->
    ok = orddict:fold(fun(Node,NodeDict,ok) ->
        % io:format("Node:~p~n", [Node])
        Cookie = dict:fetch(cookie, NodeDict),
        % io:format("Enable tracing on : ~p~n", [Node]),
        % {error,{already_started,P}} = goanna_api:add_node(Node,Cookie,tcpip_port),
        {ok,updated} = goanna_api:add_node_callbacks(Node, Cookie),
        ok
    end, ok, State#?STATE.nodes),
    {reply, true, State#?STATE{ tracing = true }};

handle_call(cluster_application_consistency, _From, State) ->
    {NodeReplies, NewState} =
        orddict:fold(fun(Node, _Value, {Replies, S}) ->
            {ok, Reply, UpdatedState} = 
                update_state(Node, S, remote_which_applications),
            {[Reply|Replies], UpdatedState}
        end, {[], State}, State#?STATE.nodes),
    % io:format("~p~n", [NodeReplies]),
    {reply, ok, NewState};
handle_call({get_db_tables, Node}, _From, State) ->
    Res = rpc:call(Node, crell_remote, get_db_tables, []),
    {reply, Res, State};
handle_call({dump_ets_tables, Node, Tables}, _From, State) ->
    F = fun() ->
        rpc:call(Node, crell_remote, dump_ets_tables, [Tables])
    end,
    {ok, Filename} = dump_tables_to_random_file(Node, Tables, F),
    {reply, Filename, State};
handle_call({dump_mnesia_tables, Node, Tables}, _From, State) ->
    F = fun() ->
        rpc:call(Node, crell_remote, dump_mnesia_tables, [Tables])
    end,
    {ok, Filename} = dump_tables_to_random_file(Node, Tables, F),
    {reply, Filename, State}.

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

terminate(_Reason, #?STATE{ nodes = Nodes } = State) ->
    orddict:fold(fun(Node=Key, RemoteStateDict, _) ->
        io:format("!!!!!!!!!!!!!!!!!! ~n~n~n terminate ~p ~n~n~n", [Node]),
        Cookie = dict:fetch(cookie, RemoteStateDict),
        E={node_disconnected, Node, Cookie},
        crell_notify:action(E),
        remove_node(Node)
    end, 0, Nodes),
    ok.
%% --------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% --------------------------------------------------------------------------

add_node(Node, Cookie, #?STATE{ nodes = N } = State) ->
    {ok, RemoteState} = start_remote_code(Node, Cookie),
    RemoteState2 = dict:store(cookie, Cookie, RemoteState),
    State#?STATE{
        nodes = orddict:store(Node, RemoteState2, N)
    }.

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

state_nodes(State) ->
    orddict:fetch_keys(State#?STATE.nodes).

-spec mktemp(Prefix) -> Result
   when Prefix   :: string(),
        Result   :: {ok, TempFile  :: file:filename()}
                  | {error, Reason :: file:posix()}.

dump_tables_to_random_file(Node, Tables, DataFun) ->
    {ok, RelDir} = file:get_cwd(),
    {ok, Filename} = mktemp(RelDir),
    DataBin = DataFun(),
    ok = file:write_file(Filename, DataBin),
    {ok, Filename}.


%% https://stackoverflow.com/questions/1222084/how-do-i-create-a-temp-filename-in-erlang
mktemp(Prefix) ->
    Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
    TempPath = filename:basedir(user_cache, Prefix),
    TempFile = filename:join(TempPath, Rand),
    Result1 = filelib:ensure_dir(TempFile),
    Result2 = file:write_file(TempFile, <<>>),
    case {Result1, Result2} of
         {ok, ok}    -> {ok, TempFile};
         {ok, Error} -> Error;
         {Error, _}  -> Error
    end.

do_remove_node(E, Node, Cookie, State) ->
    crell_notify:action(E),
    _NewState = remove_node(Node, Cookie, State).