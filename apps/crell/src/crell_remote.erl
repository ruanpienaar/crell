-module(crell_remote).
-record(db, {q, p, links, links2}).
-export([
    init/0,
    remote_appmon_pid/1,
    remote_modules/1,
    get_remote_module_functions/1,
    remote_applications/1,
    remote_all_env/1,
    calc_app_tree/2,
    calc_proc_tree/2,
    non_sys_processes/0,
    get_app_env/0,
    get_db_tables/0,
    dump_ets_tables/1,
    dump_mnesia_tables/1
]).
%%----------------------------------------------------------------------

-spec init() -> {ok, dict:dict()}.
init() ->
    init(dict:new()).

init(Dict) ->
    RRAs = running_apps(),
    Dict1 = remote_appmon_pid(Dict),
    Dict2 = remote_modules(Dict1),
    Dict3 = remote_applications(Dict2, RRAs),
    Dict4 = remote_all_env(Dict3, RRAs),
    {ok, Dict4}.

remote_appmon_pid(Dict) ->
    {ok,Pid} = start_appmon(),
    dict:store(remote_appmon_pid, Pid, Dict).

remote_modules(Dict) ->
    dict:store(remote_modules, get_remote_modules(), Dict).

get_remote_modules() ->
    % [{Mod, Mod:module_info(exports)} || {Mod, _FPath} <- code:all_loaded()].
    [{Mod, _Functions=[]} || {Mod, _FPath} <- code:all_loaded()].

get_remote_module_functions(Mod) ->
    Mod:module_info(exports).

remote_applications(Dict) ->
    dict:store(remote_running_applications, running_apps(), Dict).

remote_applications(Dict, RRAs) ->
    dict:store(remote_running_applications, RRAs, Dict).

remote_all_env(Dict) ->
    dict:store(remote_all_env, get_app_env(running_apps()), Dict).

remote_all_env(Dict,RRAs) ->
    dict:store(remote_all_env, get_app_env(RRAs), Dict).

start_appmon() ->
    %% Check erlang versions here...
    %% appmon:start().
    case whereis(appmon_info) of
        undefined ->
            appmon_info:start_link2(node(), self(), []);
        Pid ->
            {ok, Pid}
    end.

%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% BEGIN OF calc_app_tree
%%
%%  App tree is the process tree shown in the application window
%%
%%  The top (root) pid is found by calling
%%  application_controller:get_master(AppName) and this is done in
%%  calc_app_on_node (before the call to calc_app_tree).
%%
%%  We are going to add processes to the P ets and we are doing it
%%  in a two step process. First all prospect processes are put on
%%  the queue Q. Then we examine the front of Q and add this
%%  process to P if it's not already in P. Then all children of
%%  the process is put on the queue Q and the process is repeated.
%%
%%  We also maintain two link ets'es, one for primary links and
%%  one for secondary links. These databases are updated at the
%%  same time as the queue is updated with children.
%%
%%**********************************************************************
%%----------------------------------------------------------------------

-spec calc_app_tree(atom(), list()) ->
    {ok,{Root :: string(),
         P2Name :: list(),  %% Pids, with no more children
         Links :: list(),   %% Pids between Root and P2
         XLinks0 :: list()  %% Foreign pids
        }
    }.
calc_app_tree(Name, Opts) ->
    case application_controller:get_master(Name) of
        Pid when is_pid(Pid) ->
            calc_proc_tree(Pid, Opts);
        _ ->
            {ok, {[], [], [], []}}
    end.

calc_proc_tree(Pid, Opts) ->
    Mode = get_opt(info_type, Opts),
    DB = new_db(Mode, Pid),
    GL = groupl(Pid),
    R = case catch do_find_proc(Mode, DB, GL, find_avoid()) of
        {ok, DB2} ->
            {ok, {format(Pid),
                  format(ets:tab2list(DB2#db.p)),
                  format(ets:tab2list(DB2#db.links)),
                  format(ets:tab2list(DB2#db.links2))}};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    end,
    ets:delete(DB#db.p),
    ets:delete(DB#db.links),
    ets:delete(DB#db.links2),
    R.

get_opt(Name, Opts) ->
    case lists:keysearch(Name, 1, Opts) of
        {value, Val} ->
            element(2, Val);
        false ->
            default(Name)
    end.

%% not all options have default values
default(info_type)      -> link;
default(load_average)   -> true;
default(load_method)    -> time;
default(load_scale)     -> prog;
default(stay_resident)  -> false;
default(timeout)        -> 2000.

get_pid(P) when is_pid(P) -> P;
get_pid(P) when is_port(P) -> P;
get_pid(X) when is_tuple(X) -> element(2, X).


%----------------------------------------------------------------------
%%---------------------------------------------------------------------
%% Handling process trees of processses that are linked to each other

do_find_proc(Mode, DB, GL, Avoid) ->
    case get_next(DB) of
    {{value, V}, DB2} ->
        do_find_proc2(V, Mode, DB2, GL, Avoid);
    {empty, DB2} ->
        {ok, DB2}
    end.

do_find_proc2(X, Mode, DB, GL, Avoid) when is_port(X) ->
    %% There used to be a broken attempt here to handle ports,
    %% but the rest of appmon can't handle ports, so now we
    %% explicitly ignore ports.
    do_find_proc(Mode, DB, GL, Avoid);
do_find_proc2(X, Mode, DB, GL, Avoid) ->
    Xpid = get_pid(X),
    DB2 = case is_proc(DB, Xpid) of
          false ->
          add_proc(DB, Xpid),
          C1 = find_children(X, Mode),
          add_children(C1, Xpid, DB, GL, Avoid, Mode);
          _ ->
          DB
      end,
    do_find_proc(Mode, DB2, GL, Avoid).


%% Find children finds the children of a process. The method varies
%% with the selected mode (sup or link) and there are also some
%% processes that must be treated differently, notably the application
%% master.
%%
find_children(X, sup) when is_pid(X) ->
    %% This is the first (root) process of a supervision tree and it
    %% better be a supervisor, we are smoked otherwise
    supervisor:which_children(X);
find_children(X, link) when is_pid(X), node(X) /= node() ->
    [];
find_children(X, link) when is_pid(X) ->
    case process_info(X, links) of
    {links, Links} ->
        lists:reverse(Links); % OTP-4082
    _ -> []
    end;
find_children({master, X}, sup) ->
    case application_master:get_child(X) of
    {Pid, _Name} when is_pid(Pid) -> [Pid];
    Pid when is_pid(Pid) -> [Pid]
    end;
find_children({_, _X, worker, _}, sup) -> [];
find_children({_, X, supervisor, _}, sup) ->
    lists:filter(fun(Thing) ->
             Pid = get_pid(Thing),
             if
                 is_pid(Pid) -> true;
                 true -> false
             end
         end,
         supervisor:which_children(X)).


%% Add links to primary (L1) or secondary (L2) sets and return an
%% updated queue. A link is considered secondary if its endpoint is in
%% the queue of un-visited but known processes.
add_children(CList, Paren, DB, _GL, _Avoid, sup) ->
    lists:foldr(fun(C, DB2) ->
            case get_pid(C) of
                P when is_pid(P) ->
                add_prim(C, Paren, DB2);
                _ -> DB2 end end,
        DB, CList);

add_children(CList, Paren, DB, GL, Avoid, _Mode) ->
    lists:foldr(fun(C, DB2) ->
            maybe_add_child(C, Paren, DB2, GL, Avoid)
        end, DB, CList).

%% Check if the child is already in P
maybe_add_child(C, Paren, DB, GL, Avoid) ->
    case is_proc(DB, C) of
    false ->
        maybe_add_child_node(C, Paren, DB, GL, Avoid);
    _ -> DB                 % In P: no action
    end.

%% Check if process on this node
maybe_add_child_node(C, Paren, DB, GL, Avoid) ->
    if
    node(C) /= node() ->
        add_foreign(C, Paren, DB);
    true ->
        maybe_add_child_avoid(C, Paren, DB, GL, Avoid)
    end.

%% Check if child is on the avoid list
maybe_add_child_avoid(C, Paren, DB, GL, Avoid) ->
    case lists:member(C, Avoid) of
    true -> DB;
    false ->
        maybe_add_child_port(C, Paren, DB, GL)
    end.

%% Check if it is a port, then it is added
maybe_add_child_port(C, Paren, DB, GL) ->
    if
    is_port(C) ->
        add_prim(C, Paren, DB);
    true ->
        maybe_add_child_sasl(C, Paren, DB, GL)
    end.

%% Use SASL stuff if present
maybe_add_child_sasl(C, Paren, DB, GL) ->
    case check_sasl_ancestor(Paren, C) of
    yes ->                  % Primary
        add_prim(C, Paren, DB);
    no ->                   % Secondary
        add_sec(C, Paren, DB);
    dont_know ->
        maybe_add_child_gl(C, Paren, DB, GL)
    end.

%% Check group leader
maybe_add_child_gl(C, Paren, DB, GL) ->
    case cmp_groupl(GL, groupl(C)) of
    true -> maybe_add_child_sec(C, Paren, DB);
    _ -> DB
    end.

%% Check if the link should be a secondary one. Note that this part is
%% pretty much a guess.
maybe_add_child_sec(C, Paren, DB) ->
    case is_in_queue(DB, C) of
    true ->                 % Yes, secondary
        add_sec(C, Paren, DB);
    _ ->                    % Primary link
        add_prim(C, Paren, DB)
    end.

check_sasl_ancestor(Paren, C) ->
    case lists:keysearch('$ancestors', 1,
             element(2,process_info(C, dictionary))) of
    {value, {_, L}} when is_list(L) ->
        H = if
            is_atom(hd(L)) -> whereis(hd(L));
            true -> hd(L)
        end,
        if
        H == Paren -> yes;
        true -> no
        end;
    _ -> dont_know
    end.


%----------------------------------------------------------------------
%%---------------------------------------------------------------------
%% Primitives for the database DB of all links, processes and the
%% queue of not visited yet processes.

-define(add_link(C, Paren, L), ets:insert(L, {Paren, C})).

new_db(Mode, Pid) ->
    P  = ets:new(processes, [set, public]),
    L1 = ets:new(links, [bag, public]),
    L2 = ets:new(extralinks, [bag, public]),
    Q = if
        Mode =:= sup ->
            queue:in({master, Pid}, queue:new());
        true ->
            queue:in(Pid, queue:new())
    end,
    #db{q=Q, p=P, links=L1, links2=L2}.

get_next(DB) ->
    {X, Q} = queue:out(DB#db.q),
    {X, DB#db{q=Q}}.

add_proc(DB, P) ->
    ets:insert(DB#db.p, {P}).

add_prim(C, Paren, DB) ->
    ?add_link(get_pid(C), Paren, DB#db.links),
    DB#db{q=queue:in(C, DB#db.q)}.

add_foreign(C, Paren, DB) ->
    ?add_link(C, Paren, DB#db.links2),
    DB#db{q=queue:in(C, DB#db.q)}.

add_sec(C, Paren, DB) ->
    ?add_link(C, Paren, DB#db.links2),
    DB.

is_proc(#db{p=Tab}, P) ->
    ets:member(Tab, P).

is_in_queue(#db{q=Q}, P) ->
    queue:member(P, Q).

%% Group leader handling. No processes or Links to processes must be
%% added when group leaders differ. Note that catch all is needed
%% because net_sup is undefined when not networked but still present
%% in the kernel_sup child list. Blahh, didn't like that.
groupl(P) ->
    case process_info(P, group_leader) of
        {group_leader, GL} ->
            GL;
        _Other ->
            nil
    end.

cmp_groupl(_GL1, nil) -> true;
cmp_groupl(GL1, GL1) -> true;
cmp_groupl(_, _) -> false.


%% Do some intelligent guessing as to cut in the tree
find_avoid() ->
    lists:foldr(fun(X, Accu) ->
               case whereis(X) of
                   P when is_pid(P) ->
                       [P|Accu];
                   _ ->
                       Accu end
               end,
        [undefined],
        [application_controller, init, error_logger, gs,
         node_serv, appmon, appmon_a, appmon_info]).



%%----------------------------------------------------------------------
%%
%% Formats the output strings
%%
%%----------------------------------------------------------------------
format([{P} | Fs]) ->               % Process or port
    [{P, format(P)} | format(Fs)];
format([{P1, P2} | Fs]) ->          % Link
    [{format(P1), format(P2)} | format(Fs)];
format([]) -> [];
format(P) when is_pid(P), node(P) /= node() ->
    pid_to_list(P) ++ " " ++ atom_to_list(node(P));
format(P) when is_pid(P) ->
    case process_info(P, registered_name) of
    {registered_name, Name} -> atom_to_list(Name);
    _ -> pid_to_list(P)
    end;
format(P) when is_port(P) ->
    "port " ++ integer_to_list(element(2, erlang:port_info(P, id)));
format(X) ->
    io:format("What: ~p~n", [X]),
    "???".


%%----------------------------------------------------------------------
%%**********************************************************************
%%
%%
%% END OF calc_app_tree
%%
%%
%%**********************************************************************
%%----------------------------------------------------------------------

non_sys_processes() ->
    non_sys_processes(processes(), []).

non_sys_processes([], R) ->
    R;
non_sys_processes([H|T], R) when H == self() ->
    non_sys_processes(T, R);
non_sys_processes([H|T], R) ->
    case process_info(H, [registered_name,initial_call,message_queue_len]) of
        undefined ->
            non_sys_processes(T, R);
        [{registered_name,Reg},{initial_call,Initial},{message_queue_len,Qlen}] ->
            Name =
                case Reg of
                    [] -> initial_call(Initial, H);
                    _  -> Reg
                end,
            case lists:member(Name, sys_processes()) of
                true ->
                    non_sys_processes(T, R);
                false ->
                    non_sys_processes(T, [ [{pid,H},{name,Name},{mq,Qlen}] |R])
            end
    end.

initial_call({proc_lib, init_p, _}, Pid) ->
    proc_lib:translate_initial_call(Pid);
initial_call(Initial, _Pid) ->
    Initial.

sys_processes() ->
    [auth, code_server, global_name_server, inet_db,
     mnesia_recover, net_kernel, timer_server, wxe_master].

running_apps() ->
    application:which_applications().

get_app_env() ->
    RRAs = running_apps(),
    get_app_env(RRAs).

get_app_env(RRAs) ->
    [{App, application:get_all_env(App)} || {App,_,_} <- RRAs ].


get_db_tables() ->
    ETbls = [{ets, tables(ets)}],
    MTbls =
        case whereis(mnesia_tm) of
            undefined ->
                [];
            _Pid ->
                [{mnesia, tables(mnesia)}]
        end,
    ETbls++MTbls.

%% COPIED FROM observer_backend.erl ( from runtime_tools )
tables(ets) ->
    Opts = [{unread_hidden, true},
            {sys_hidden, false}
    ],
    HideUnread = proplists:get_value(unread_hidden, Opts, true),
    HideSys = proplists:get_value(sys_hidden, Opts, true),
    Fun = fun(Id, Acc) ->
        try
            TabId =
                case ets:info(Id, named_table) of
                    true -> ignore;
                    false -> Id
                end,
            Name = ets:info(Id, name),
            Protection = ets:info(Id, protection),
            ignore(HideUnread andalso Protection == private, unreadable),
            Owner = ets:info(Id, owner),
            RegName =
                case catch process_info(Owner, registered_name) of
                    [] -> ignore;
                    {registered_name, ProcName} -> ProcName
                end,
            ignore(HideSys andalso ordsets:is_element(RegName, sys_processes()), system_tab),
            ignore(HideSys andalso ordsets:is_element(Name, sys_tables()), system_tab),
            ignore((RegName == mnesia_monitor)
            andalso Name /= schema
            andalso is_atom((catch mnesia:table_info(Name, where_to_read))), mnesia_tab),
            Memory = ets:info(Id, memory) * erlang:system_info(wordsize),
            Tab = [{name,Name},
                   {id,TabId},
                   {protection,Protection},
                   {owner,Owner},
                   {size,ets:info(Id, size)},
                   {reg_name,RegName},
                   {type,ets:info(Id, type)},
                   {keypos,ets:info(Id, keypos)},
                   {heir,ets:info(Id, heir)},
                   {memory,Memory},
                   {compressed,ets:info(Id, compressed)},
                   {fixed,ets:info(Id, fixed)}
                  ],
            [Tab|Acc]
        catch _:_What ->
            Acc
        end
    end,
    lists:foldl(Fun, [], ets:all());
tables(mnesia) ->
    Opts = [{sys_hidden, false}
    ],
    HideSys = proplists:get_value(sys_hidden, Opts, true),
    Owner = ets:info(schema, owner),
    Owner /= undefined orelse
    throw({error, "Mnesia is not running on: " ++ atom_to_list(node())}),
    {registered_name, RegName} = process_info(Owner, registered_name),
    Fun = fun(Id, Acc) ->
        try
            Name = Id,
            ignore(HideSys andalso ordsets:is_element(Name, mnesia_tables()), system_tab),
            ignore(Name =:= schema, mnesia_tab),
            Storage = mnesia:table_info(Id, storage_type),
            Tab0 = [{name,Name},
                    {owner,Owner},
                    {size,mnesia:table_info(Id, size)},
                    {reg_name,RegName},
                    {type,mnesia:table_info(Id, type)},
                    {keypos,2},
                    {memory,mnesia:table_info(Id, memory) * erlang:system_info(wordsize)},
                    {storage,Storage},
                    {index,mnesia:table_info(Id, index)}
                   ],
            Tab =
                if Storage == disc_only_copies ->
                    [{fixed, dets:info(Id, safe_fixed)}|Tab0];
                   (Storage == ram_copies) orelse (Storage == disc_copies) ->
                    [{fixed, ets:info(Id, fixed)},
                     {compressed, ets:info(Id, compressed)}|Tab0];
                true ->
                    Tab0
                end,
            [Tab|Acc]
        catch _:_What ->
            Acc
        end
    end,
    lists:foldl(Fun, [], mnesia:system_info(tables)).

ignore(true, Reason) -> throw(Reason);
ignore(_,_ ) -> ok.

sys_tables() ->
    [ac_tab,  asn1,
     cdv_dump_index_table,  cdv_menu_table,  cdv_decode_heap_table,
     cell_id,  cell_pos,  clist,
     cover_internal_data_table,   cover_collected_remote_data_table, cover_binary_code_table,
     code, code_names,  cookies,
     corba_policy,  corba_policy_associations,
     dets, dets_owners, dets_registry,
     disk_log_names, disk_log_pids,
     eprof,  erl_atom_cache, erl_epmd_nodes,
     etop_accum_tab,  etop_tr,
     ets_coverage_data,
     file_io_servers,
     gs_mapping, gs_names,  gstk_db,
     gstk_grid_cellid, gstk_grid_cellpos, gstk_grid_id,
     httpd,
     id,
     ign_req_index, ign_requests,
     index,
     inet_cache, inet_db, inet_hosts,
     'InitialReferences',
     int_db,
     interpreter_includedirs_macros,
     ir_WstringDef,
     lmcounter,  locks,
                        %     mnesia_decision,
     mnesia_gvar, mnesia_stats,
                        %     mnesia_transient_decision,
     pg2_table,
     queue,
     schema,
     shell_records,
     snmp_agent_table, snmp_local_db2, snmp_mib_data, snmp_note_store, snmp_symbolic_ets,
     tkFun, tkLink, tkPriv,
     ttb, ttb_history_table,
     udp_fds, udp_pids
    ].

mnesia_tables() ->
    [ir_AliasDef, ir_ArrayDef, ir_AttributeDef, ir_ConstantDef,
     ir_Contained, ir_Container, ir_EnumDef, ir_ExceptionDef,
     ir_IDLType, ir_IRObject, ir_InterfaceDef, ir_ModuleDef,
     ir_ORB, ir_OperationDef, ir_PrimitiveDef, ir_Repository,
     ir_SequenceDef, ir_StringDef, ir_StructDef, ir_TypedefDef,
     ir_UnionDef, logTable, logTransferTable, mesh_meas,
     mesh_type, mnesia_clist, orber_CosNaming,
     orber_objkeys, user
    ].

dump_ets_tables(Tables) when is_list(Tables) ->
    dump_ets_tables(Tables, []).

dump_ets_tables([], R) ->
    lists:reverse(R);
dump_ets_tables([H|T], R) ->
    dump_ets_tables(T, [try_dump_ets(H)|R]).

try_dump_ets(Table) ->
    try
        list_to_binary(io_lib:format("~p~n", [{Table, ets:tab2list(Table)}]))
    catch
        C:E ->
            R={Table, {C, E, erlang:get_stacktrace()}},
            list_to_binary( io_lib:format("~p~n", [R]) )
    end.

dump_mnesia_tables(Tables) ->
    mnesia:info(),
    L = lists:map(fun(Table) ->
        {atomic, Res} = mnesia:transaction(fun() ->
            reading_mnesia_entries(Table)
        end),
        {Table, Res}
    end, Tables),
    list_to_binary(io_lib:format("~p", [L])).

reading_mnesia_entries(Table) ->
    reading_mnesia_entries(Table, started, []).

reading_mnesia_entries(Table, '$end_of_table', R) ->
    lists:reverse(R);
reading_mnesia_entries(Table, started, []) ->
    reading_mnesia_entries(Table, mnesia:first(Table), []);
reading_mnesia_entries(Table, Key, R) ->
    reading_mnesia_entries(
        Table,
        mnesia:next(Table, Key),
        [mread(Table, Key)|R]
    ).

mread(T, K) ->
    [V] = mnesia:read(T, K),
    V.