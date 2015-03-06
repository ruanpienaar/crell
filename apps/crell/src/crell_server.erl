-module (crell_server).
-export([start_link/0,
         calc_app/1,
         calc_app/2,
         calc_proc/1,
         calc_proc/2
        ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, crell_server_state).
-record(?STATE, { appmon_pid
                }).

-record(db, {q, p, links, links2}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

calc_app(App) ->
    calc_app(App,[]).

calc_app(App,Opts) ->
    gen_server:call(?MODULE, {app,App,Opts}).

calc_proc(Pid) ->
    calc_proc(Pid,[]).

calc_proc(Pid,Opts) ->
    gen_server:call(?MODULE, {pid,Pid,Opts}).

%% ---------------------------------------

init({}) ->
    {ok,AppMonPid} = appmon_info:start_link(node(), self(), []),
    {ok, #?STATE{appmon_pid = AppMonPid}}.

handle_call({app,App,Opts}, _From, State) ->
    R = calc_app_tree(App, Opts),
    {reply, R, State};
handle_call({pid,Pid,Opts}, _From, State) ->
    R = calc_proc_tree(Pid, Opts),
    {reply, R, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call, ?MODULE}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Shell got {delivery,<0.33.0>,app,kernel,
%                     {"<0.8.0>",
%                      [{<0.12.0>,"global_name_server"},
%                       {<0.22.0>,"user_drv"},
%                       {<0.19.0>,"standard_error_sup"},
%                       {<0.23.0>,"user"},
%                       {<0.24.0>,"<0.24.0>"},
%                       {<0.16.0>,"global_group"},
%                       {<0.15.0>,"inet_db"},
%                       {<0.18.0>,"code_server"},
%                       {<0.14.0>,"<0.14.0>"},
%                       {<0.21.0>,"<0.21.0>"},
%                       {<0.10.0>,"kernel_sup"},
%                       {<0.11.0>,"rex"},
%                       {<0.8.0>,"<0.8.0>"},
%                       {<0.17.0>,"file_server_2"},
%                       {<0.13.0>,"<0.13.0>"},
%                       {<0.9.0>,"<0.9.0>"},
%                       {<0.20.0>,"standard_error"},
%                       {<0.26.0>,"<0.26.0>"},
%                       {<0.27.0>,"kernel_safe_sup"}],
%                      [{"global_name_server","<0.13.0>"},
%                       {"global_name_server","<0.14.0>"},
%                       {"user_drv","<0.24.0>"},
%                       {"user_drv","port 400"},
%                       {"standard_error_sup","standard_error"},
%                       {"user","user_drv"},
%                       {"<0.21.0>","user"},
%                       {"kernel_sup","inet_db"},
%                       {"kernel_sup","standard_error_sup"},
%                       {"kernel_sup","<0.26.0>"},
%                       {"kernel_sup","kernel_safe_sup"},
%                       {"kernel_sup","<0.21.0>"},
%                       {"kernel_sup","file_server_2"},
%                       {"kernel_sup","code_server"},
%                       {"kernel_sup","global_group"},
%                       {"kernel_sup","rex"},
%                       {"kernel_sup","global_name_server"},
%                       {"<0.8.0>","<0.9.0>"},
%                       {"file_server_2","port 47"},
%                       {"<0.9.0>","kernel_sup"},
%                       {"standard_error","port 391"}],
%                      []}}





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

%% !!! NEW
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

