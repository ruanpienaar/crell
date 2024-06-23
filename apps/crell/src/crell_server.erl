% TODO: Start crell server per node
% TODO: make crell_server configurable:
%   poll_server&no-inject
%   call_remote&inject
%
-module (crell_server).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(STATE, crell_server_state).
% TODO: change tracing from boolean to map #{Node => TracingBool, Node2 => Bool }
-record(?STATE, {
    nodes=orddict:new(), % orddict OF dict's (the nodes)
    tracing=false,
    cluster_modules
}).

-export([
    start_link/0,
    add_node/2,
    remove_node/1,
    nodes/0,
    clusters/0,
    connecting_nodes/0,
    runtime_modules/1,
    cluster_modules/0,
    cluster_module_functions/1,
    % runtime_module_functions/2,
    module_source/2,
    non_sys_processes/1,
    calc_app/2,
    calc_app/3,
    calc_proc/2,
    calc_proc/3,
    calc_app_env/2,
    remote_which_applications/1,
    next_xref_branch/1,
    remote_pid_info/2
]).

% db
-export([
    get_db_tables/1,
    dump_ets_tables/2,
    dump_mnesia_tables/2
]).

% tracing
-export([
    is_tracing/0,
    toggle_tracing/1
    % toggle_cluster_tracing/1
]).

% % inject
% -export([
%     inject_module/2,
%     purge_module/2
% ]).

% cluster
-export([
    cluster_application_consistency/0,
    discover_neighbour_nodes/1
]).

% recon
-export([
    recon_inj_status/0
]).

%% TODO: choose a better data structure, or use ets...
%% The orddict, is not coping with the amounts of data from the module function exports...

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%% TODO: extremely slow adding nodes, block other nodes from being added...FIXME
add_node(Node, Cookie) ->
    {atomic, ok} = crell_nodes:create(crell_nodes:new(Node, Cookie)),
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
                    {ok,{_, updated}} = hawk:add_connect_callback(Node, {NC,CCB}),
                    {ok,{_, updated}} = hawk:add_disconnect_callback(Node, {ND,DCB}),
                    ok
            end;
        {error, no_such_node} ->
            {ok,_} = hawk:add_node(Node, Cookie, ConnectedCallBack, DisconnCallBack),
            ok;
        {error, connecting} ->
            %% TODO: log that the node is connecting..
            ok
    end.

remove_node(Node) ->
    {atomic, ok} = crell_nodes:delete(Node),
    ok = spike:purge(Node, crell_remote),
    ok = hawk:remove_node(Node),
    crell_notify:action({node_deleted, Node}),
    ok.

nodes() ->
    gen_server:call(?MODULE, nodes, infinity).

clusters() ->
    crell_nodes:all_clusters().

%% TODO: could've also casted, and then sent the response with grpoc as a event.
connecting_nodes() ->
    gen_server:call(?MODULE, connecting_nodes).

%% Maybe we should bundle modules per application ?

runtime_modules(Node) ->
    gen_server:call(?MODULE, {runtime_modules, Node}).

cluster_modules() ->
    gen_server:call(?MODULE, cluster_modules).

cluster_module_functions(Mod) ->
    gen_server:call(?MODULE, {cluster_module_functions, Mod}).

% runtime_module_functions(Node, Mod) ->
%     gen_server:call(?MODULE, {runtime_module_functions, Node, Mod}).

module_source(Node, Module) ->
    gen_server:call(?MODULE, {module_source, Node, Module}).

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
next_xref_branch(_Node) ->
    ok.

remote_pid_info(Node, Pid) ->
    gen_server:call(?MODULE, {remote_pid_info, Node, Pid}).

get_db_tables(Node) ->
    gen_server:call(?MODULE, {get_db_tables, Node}).

dump_ets_tables(Node, Tables) ->
    gen_server:call(?MODULE, {dump_ets_tables, Node, Tables}, infinity).

dump_mnesia_tables(Node, Tables) ->
    gen_server:call(?MODULE, {dump_mnesia_tables, Node, Tables}, infinity).

is_tracing() ->
    gen_server:call(?MODULE, is_tracing).

toggle_tracing(Node) ->
    gen_server:call(?MODULE, {toggle_tracing, Node}).

cluster_application_consistency() ->
    gen_server:call(?MODULE, cluster_application_consistency).

discover_neighbour_nodes(Nodes) ->
    AllNeighbours =
        gen_server:call(?MODULE, {discover_neighbour_nodes, Nodes}),
    lists:foreach(fun({Node, Cookie, Neighbours}) ->
        lists:foreach(fun(NeighbourNode) ->
            %% TODO: maybe some nodes have diff cookies...?
            %% might not be able to connect to the other nodes with orig cookie.
            logger:info(#{ adding_neighbouring_node => [Node, NeighbourNode, Cookie]}),
            ok = add_node(NeighbourNode, Cookie)
        end, Neighbours)
    end, AllNeighbours).

recon_inj_status() ->
    gen_server:call(?MODULE, recon_inj_status).

%% ---------------------------------------

init({}) ->
    %% TODO: maybe check with Hawk, who's already connected...
    process_flag(trap_exit, true),

    %% add the old nodes:
    lists:foreach(fun(NodeRec) ->
        NP = crell_nodes:obj_to_proplist(NodeRec),
        {node, Node} = lists:keyfind(node, 1, NP),
        {cookie, Cookie} = lists:keyfind(cookie, 1, NP),
        ok = add_node(Node, Cookie)
    end, crell_nodes:all()),

    {ok, #?STATE{}}.

update_state(Node, #?STATE{nodes = Nodes } = State, LookupValue) ->
    case orddict:find(Node, Nodes) of
        {ok, RemoteState} ->
            {ok, Response, UpdatedRemoteState} = do_get_values(Node, RemoteState, LookupValue),
            UpdatedState = State#?STATE{nodes = orddict:store(Node, UpdatedRemoteState, Nodes)},
            {ok, Response, UpdatedState};
        error ->
            {ok, {error, {unknown_node, Node}}, State}
    end.

%% TODO: rather make explicit remote update calls, update_remote_modules
do_get_values(_Node, RemoteState, get_remote_modules) ->
    % TODO: Unused
    RemoteModules = dict:fetch(remote_modules, RemoteState),
    Mods = lists:sort(
        lists:map(fun({Mod, _Exports}) -> Mod end, RemoteModules)
    ),
    {ok, Mods, RemoteState};
do_get_values(Node, RemoteState, {get_remote_module_functions, Mod}) ->
    RemoteModules = dict:fetch(remote_modules, RemoteState),
    case lists:keyfind(Mod, 1, RemoteModules) of
        false ->
            %% Maybe update when not found...
            {ok, false, RemoteState};
        {Mod, []} ->
            ModFunctions = rpc:call(Node, crell_remote, get_remote_module_functions, [Mod]),
            SortedFunctions = lists:sort(ModFunctions),
            UpdatedModFuncs = lists:keyreplace(Mod, 1, RemoteModules, {Mod, SortedFunctions}),
            RemoteState2 = dict:store(remote_modules, UpdatedModFuncs, RemoteState),
            {ok, SortedFunctions, RemoteState2};
        % Hoping that the remote code, won't change too much.
        {Mod, Functions} ->
            {ok, Functions, RemoteState}
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
handle_call(connecting_nodes, _From, State) ->
    {reply, hawk:nodes() -- state_nodes(State), State};
handle_call({runtime_modules, Node}, _From, State) ->
    % Update state here
    % {ok, Reply, NewState} = update_state(Node, State, get_remote_modules),
    % {reply, Reply, NewState};
    ModsBin = rpc:call(Node, crell_remote, get_remote_modules2, []),
    {reply, erlang:binary_to_term(ModsBin), State};
handle_call(cluster_modules, _From, State) ->
    %% TODO: we need to make a update cluster_state function
    %% TODO: group erlang nodes together based on a cookie/some-family, some label??
    %% Aggregate the nodes' modules, check consistency, and update state.

    %% TODO: hack, just use the last Node's modules...
    LastNode = lists:last(orddict:fetch_keys(State#?STATE.nodes)),
    {ok, Reply, NewState} = update_state(LastNode, State, get_remote_modules),
    {reply, Reply, NewState};
handle_call({cluster_module_functions, Mod}, _From, State) ->
    %% TODO: hack, just use the last Node's modules...
    LastNode = lists:last(orddict:fetch_keys(State#?STATE.nodes)),
    {ok, Reply, NewState} = update_state(LastNode, State, {get_remote_module_functions, Mod}),
    {reply, Reply, NewState};
% handle_call({runtime_module_functions, Node, Mod}, _From, State) ->
%     % Update state here
%     {ok, Reply, _} = update_state(Node, State, {get_remote_module_functions, Mod}),
%     {reply, Reply, State};
handle_call({module_source, Node, Module}, _From, State) ->
    %% TODO: build a function code getter. exported functions.
    %% TODO: Add error handling
    % logger:info(#{getting_source_for => Module }),
    % Source = case get_source(Module) of
    %     {ok, CodeString} ->
    %         % logger:info(io_lib:format("~s", [CodeString])),
    %         % {ok, list_to_binary(io_lib:format("~s", [CodeString]))};
    %         {ok, list_to_binary(CodeString)};
    %     X ->
    %         logger:error(#{error => X }),
    %         {ok, <<"error">>}
    % end,
    Code =
        case rpc:call(Node, crell_remote, get_source, [Module]) of
            {ok, S} ->
                list_to_binary(S);
            {error, E} ->
                list_to_binary(io_lib:format("error ~p", [E]))
        end,
    {reply, {ok, Code}, State};
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
handle_call({remote_pid_info, Node, Pid}, _From, State) ->
    Reply = rpc:call(Node, erlang, process_info, [Pid]),
    {reply, {ok, Reply}, State};
handle_call({proc, Node, Pid, _Opts}, _From, State) ->
    ProcInfo = rpc:call(Node, erlang, process_info, [Pid]),
    {reply, ProcInfo, State};
handle_call(E={node_connected, Node, Cookie}, _From, State) ->
    crell_notify:action({node_connecting, Node}),
    {ok, RemoteState} = start_remote_code(Node, Cookie),
    RemoteState2 = dict:store(cookie, Cookie, RemoteState),
    {atomic,ok} = crell_nodes:node_connected(Node),
    crell_notify:action(E),
    {reply, ok, State#?STATE{
        nodes = orddict:store(Node, RemoteState2, State#?STATE.nodes)
    }};
handle_call(E={node_disconnected, Node, _Cookie}, _From, State) ->
    crell_notify:action(E),
    {reply, ok, State#?STATE{nodes = orddict:erase(Node, State#?STATE.nodes)}};
handle_call(is_tracing, _From, State) ->
    {reply, State#?STATE.tracing, State};
%% TODO: maybe add some more conditions,
handle_call({toggle_tracing, Node}, _From, #?STATE{tracing=true} = State) ->
    ok = goanna_api:stop_trace(),
    true = goanna_api:remove_goanna_callbacks(Node),
    ok = goanna_api:remove_goanna_node(Node),
    {reply, false, State#?STATE{tracing = false }};
handle_call({toggle_tracing, Node}, _From, #?STATE{tracing=false} = State) ->
    NodeDict = orddict:fetch(Node, State#?STATE.nodes),
    Cookie = dict:fetch(cookie, NodeDict),
    {ok, _} = goanna_api:add_node_callbacks(Node, Cookie),
    {reply, true, State#?STATE{tracing = true }};
% TODO: Add cluster_tracing status
handle_call({toggle_cluster_tracing, _Cluster}, _From, State) ->
    %% Enable part:
    % ok = orddict:fold(fun(Node,NodeDict,ok) ->
    %     Cookie = dict:fetch(cookie, NodeDict),
    %     goanna_api:add_node_callbacks(Node, Cookie),
    %     ok
    % end, ok, State#?STATE.nodes),
    %% Disable part:
    % ok = orddict:fold(fun(Node,_NodeDict,ok) ->
    %     % Don't match  on these calls, nodes could be added while other nodes are tracing
    %     goanna_api:stop_trace(),
    %     goanna_api:remove_goanna_callbacks(Node),
    %     goanna_api:remove_goanna_node(Node),
    %     ok
    % end, ok, State#?STATE.nodes),
    {reply, ok, State};
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
    {reply, Filename, State};

handle_call(cluster_application_consistency, _From, State) ->
    {NodeReplies, NewState} =
        orddict:fold(fun(Node, _Value, {Replies, S}) ->
            {ok, Reply, UpdatedState} =
                update_state(Node, S, remote_which_applications),
            {[Reply|Replies], UpdatedState}
        end, {[], State}, State#?STATE.nodes),
    logger:info(#{cluster_node_consistency => NodeReplies}),
    {reply, ok, NewState};

handle_call({discover_neighbour_nodes, AllNodes}, _From,
        #?STATE{nodes = Nodes } = State) ->
    {reply,
    lists:foldl(fun(Node, Acc) ->
        % update_state has the same lookup.. maybe abstract into 1 func call?
        case orddict:find(Node, Nodes) of
            {ok, NodeDict} ->
                Cookie = dict:fetch(cookie, NodeDict),
                case rpc:call(Node, erlang, nodes, []) of
                    [] ->
                        Acc;
                    Neighbours ->
                        [{Node,
                          Cookie,
                          lists:foldl(fun(NN, NeighAcc) ->
                              case orddict:find(NN, Nodes) of
                                error ->
                                    [NN|NeighAcc];
                                {ok, _} ->
                                    NeighAcc
                              end
                          end, [], Neighbours)
                         }|Acc]
                end;
            error ->
                Acc
        end
    end, [], AllNodes),
    State};

handle_call(recon_inj_status, _From, State) ->
    {reply, false, State}.
%% --------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------------

handle_info(Info, State) ->
    logger:info(#{ unhandled_info => Info }),
    {noreply, State}.
%% --------------------------------------------------------------------------

terminate(_Reason, #?STATE{nodes = Nodes } = State) ->
    orddict:fold(fun(Node, RemoteStateDict, _) ->
        Cookie = dict:fetch(cookie, RemoteStateDict),
        _E={node_disconnected, Node, Cookie}
        % TODO: why is this action commented?
        %% crell_notify:action(E),
        %% remove_node(Node)
    end, 0, Nodes),
    logger:notice(#{terminate_state => State}).
%% --------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% --------------------------------------------------------------------------

start_remote_code(Node,_Cookie) ->
    case application:get_env(crell, remote_action) of
        {ok, 1} ->
            % try loading module
            case spike:inject(Node, crell_remote) of
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

% TODO: this has been moved into crell_remote.erl
% -spec abstract_code(module()) -> [erl_parse:abstract_form()].
% abstract_code(Module) ->
%     File = code:which(Module),
%     logger:info(#{ code_which => File }),
%     {ok,{_Mod,[{abstract_code,{_Version,Forms}}]}} = beam_lib:chunks(File, [abstract_code]),
%     Forms.
% mod_src(Module) ->
%     Forms = abstract_code(Module),
%     lists:flatten([[erl_pp:form(F),$\n] || F <- Forms, element(1,F) =:= attribute orelse element(1,F) =:= function]).
% fun_src(Mod, Fun, Arity) ->
%     Forms = abstract_code(Mod),
%     [FF] = [FF || FF = {function, _Line, Fun2, Arity2, _} <- Forms, Fun2 =:= Fun, Arity2 =:= Arity],
%     lists:flatten(erl_pp:form(FF)).
% -spec get_source(module() | {module(), function(), Arity :: non_neg_integer()}) -> {ok, string()} | {error, Reason :: any()}.
% get_source(What) ->
%     try
%         case What of
%                 {M,F,A} ->
%                         {ok, fun_src(M,F,A)};
%                 Mod when is_atom(Mod)->
%                         {ok, mod_src(Mod)}
%         end
%         catch _:E:S ->
%                 {error, {E, S}}
%     end.

state_nodes(State) ->
    orddict:fetch_keys(State#?STATE.nodes).

% -spec mktemp(Prefix) -> Result
%    when Prefix   :: string(),
%         Result   :: {ok, TempFile  :: file:filename()}
%                   | {error, Reason :: file:posix()}.

dump_tables_to_random_file(Node, Tables, DataFun) ->
    % {ok, RelDir} = file:get_cwd(),
    DownloadDir = code:priv_dir(crell_web)++"/www/dl",
    {ok, Filename} = rand_file(DownloadDir),
    DataBin = DataFun(),
    % io:format("~p~n", [DataBin]),
    ok = file:write_file(Filename, DataBin),
    {ok, "dl/"++filename:basename(Filename)}.

rand_file(Prefix) ->
    Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36)++".txt",
    TempFile = filename:join(Prefix, Rand),
    ok = file:write_file(TempFile, <<>>),
    {ok, TempFile}.

%% https://stackoverflow.com/questions/1222084/how-do-i-create-a-temp-filename-in-erlang
%mktemp(Prefix) ->
%    Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36)++".txt",
%    TempPath = filename:basedir(user_cache, Prefix),
%    TempFile = filename:join(TempPath, Rand),
%    Result1 = filelib:ensure_dir(TempFile),
%    Result2 = file:write_file(TempFile, <<>>),
%    case {Result1, Result2} of
%         {ok, ok}    -> {ok, TempFile};
%         {ok, Error} -> Error;
%         {Error, _}  -> Error
%    end.