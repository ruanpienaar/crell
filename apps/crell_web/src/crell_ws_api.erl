-module(crell_ws_api).


-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).


-define(STATE, crell_ws_api).
-record(?STATE, {}).

init(Req, _Opts) ->
    true = crell_notify:subscribe({node_events}),
    process_flag(trap_exit, true),
    {cowboy_websocket, Req, #?STATE{}}.

websocket_handle({text, ReqJson}, Req, State) ->
    case jsx:decode(ReqJson) of
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"nodes">>},
         {<<"args">>,[]}] ->
            NodesJson = nodes_json(),
            {reply, {text, NodesJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"connecting_nodes">>},
         {<<"args">>,[]}] ->
            ConnectingNodesJson = connecting_nodes_json(),
            {reply, {text, ConnectingNodesJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"remote_which_applications">>},
         {<<"args">>,[Node]}] ->
            NodeAppsJson = get_node_apps_json(Node),
            {reply, {text, NodeAppsJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"add_node">>},
         {<<"args">>,[Node, Cookie]}] ->
            ok = crell_server:add_node(crell_web_utils:ens_atom(Node),
                                       crell_web_utils:ens_atom(Cookie)),
            ConnectingNodesJson = connecting_nodes_json(),
            {reply, {text, ConnectingNodesJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,DeleteFunc},
         {<<"args">>,Nodes}] when DeleteFunc == <<"del_node">> orelse
                                  DeleteFunc == <<"conn_del_node">> ->
            ok = lists:foreach(fun(Node) ->
                ok = crell_server:remove_node(
                    crell_web_utils:ens_atom(Node)
                )
            end, Nodes),
            {reply, reply_ok(), Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"non_sys_processes">>},
         {<<"args">>,[Node]}] ->
            PidsJson = pids_json(crell_server:non_sys_processes(
                crell_web_utils:ens_atom(Node))
            ),
            {reply, {text, PidsJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"runtime_modules">>},
         {<<"args">>,[Node]}] ->
            Mods = crell_server:runtime_modules(
                crell_web_utils:ens_atom(Node)
            ),
            ModsJson = mods_json(Mods, []),
            {reply, {text, ModsJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"is_tracing">>},
         {<<"args">>,[]}] ->
            {reply, reply("is_tracing", crell_server:is_tracing()), Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"toggle_tracing">>},
         {<<"args">>,[]}] ->
            {reply, reply("is_tracing", crell_server:toggle_tracing()), Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"cluster_modules">>},
         {<<"args">>,[]}] ->
            Mods = crell_server:cluster_modules(),
            ModsJson = mods_json(Mods, []),
            {reply, {text, ModsJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"cluster_module_functions">>},
         {<<"args">>,[Mod]}] ->
            ModsFuncs = crell_server:cluster_module_functions(
                crell_web_utils:ens_atom(Mod)
            ),
            ModsFuncJson = mod_funcs_json(ModsFuncs, []),
            {reply, {text, ModsFuncJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"calc_app_env">>},
         {<<"args">>,[Node, AppName]}] ->
            AppEnvJson = app_env_json(
                AppName,
                crell_server:calc_app_env(
                    crell_web_utils:ens_atom(Node),
                    crell_web_utils:ens_atom(AppName)
                )
            ),
            {reply, {text, AppEnvJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"get_db_tables">>},
         {<<"args">>,[Node]}] ->
            DbTypeTables =
                crell_server:get_db_tables(
                    crell_web_utils:ens_atom(Node)
                ),
            {reply, {text, db_info_json(DbTypeTables)}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"dump_ets_tables">>},
         {<<"args">>,[Node, Tbls]}] ->
            AtomTbls = [ list_to_atom(binary_to_list(T)) || T <- Tbls ],
            DLFilename =
                crell_server:dump_ets_tables(
                    crell_web_utils:ens_atom(Node),
                    AtomTbls
                ),
            DownloadJson = jsx:encode([{
                <<"ets_dl_url">>,
                crell_web_utils:ens_atom(DLFilename)
            }]),
            {reply, {text, DownloadJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"dump_mnesia_tables">>},
         {<<"args">>,[Node, Tbls]}] ->
            AtomTbls = [ list_to_atom(binary_to_list(T)) || T <- Tbls ],
            DLFilename =
                crell_server:dump_mnesia_tables(
                    crell_web_utils:ens_atom(Node),
                    AtomTbls
                ),
            DownloadJson = jsx:encode([{
                <<"mnesia_dl_url">>,
                crell_web_utils:ens_atom(DLFilename)
            }]),
            {reply, {text, DownloadJson}, Req, State};
    	[{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"cluster_application_consistency">>},
         {<<"args">>,[]}] ->
            {reply, reply('cluster_application_consistency',
                         crell_server:cluster_application_consistency()),
                Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"recon_inj_status">>},
         {<<"args">>,[]}] ->
            {reply, reply('recon_inj_status',
                          crell_server:recon_inj_status()),
                Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"discover_neighbour_nodes">>},
         {<<"args">>,Nodes}] ->
            ok = crell_server:discover_neighbour_nodes(
                [ crell_web_utils:ens_atom(Node) || Node <- Nodes ]
            ),
            NodesJson = nodes_json(),
            {reply, {text, NodesJson}, Req, State};
        UnknownJson ->
            io:format("[~p] UnknownJson: ~p~n", [?MODULE,UnknownJson]),
            Json = jsx:encode([{<<"unknown_json">>, UnknownJson}]),
            {reply, {text, Json}, Req, State}
    end.

reply_ok() ->
    reply(reply, ok).

reply(Reply, Val) ->
    {text, jsx:encode([{crell_web_utils:ens_bin(Reply),
                        crell_web_utils:ens_bin(Val)}])}.

% websocket_info(nodes, Req, State) ->
%     io:format("Websocket info nodes\n"),
%     NodesJson = nodes_json(),
%     {reply, {text, NodesJson}, Req, State};
websocket_info({crell_notify,
                {node_events},
                {node_connecting, Node}}, Req, State) ->
    {reply, {text, jsx:encode([{<<"node_connecting">>,
                                crell_web_utils:ens_bin(Node)}])}, Req, State};
websocket_info({crell_notify,
                {node_events},
                {node_deleted, Node}}, Req, State) ->
    {reply, {text, jsx:encode([{<<"node_deleted">>,
                                crell_web_utils:ens_bin(Node)}])}, Req, State};
websocket_info({crell_notify,
                {node_events},
                {Event,Node,_Cookie}
               }, Req, State) when Event == node_connected;
                                   Event == node_disconnected ->
    {reply, {text, jsx:encode([{crell_web_utils:ens_bin(Event),
                                crell_web_utils:ens_bin(Node)}])}, Req, State};
websocket_info({timeout, _Ref, <<"nodes">>}, Req, State) ->
    NodesJson = nodes_json(),
    {reply, {text, NodesJson}, Req, State};
websocket_info(Info, Req, State) ->
    io:format("Info : ~p\n", [Info]),
    Json = <<"reply">>,
    {reply, {text, Json}, Req, State}.

nodes_json() ->
    jsx:encode([{<<"nodes">>,
        [ Node || Node <- crell_server:nodes() ]}]).

connecting_nodes_json() ->
    jsx:encode([{<<"connecting_nodes">>,
        [ Node || Node <- crell_server:connecting_nodes() ]}]).

get_node_apps_json(Node) ->
    NodeApps = crell_server:remote_which_applications(crell_web_utils:ens_atom(Node)),
    NodeAppsJson = [ [{<<"name">>, crell_web_utils:ens_bin(AppName)}
                    , {<<"erts_vsn">>, crell_web_utils:ens_bin(ErtsVsn)}
                    , {<<"vsn">>, crell_web_utils:ens_bin(Vsn)}
                    ] || {AppName, ErtsVsn, Vsn} <- NodeApps],
    jsx:encode([{<<"node_apps">>, NodeAppsJson}]).

pids_json(Pids) ->
    pids_json(Pids, []).

pids_json([], R) ->
    jsx:encode([{<<"pids">>, lists:reverse(R)}]);
pids_json([[{pid,P},{name,N},{mq,M}]|T], R) ->
    pids_json(T, [[{<<"pid">>, crell_web_utils:ens_bin(P)},
                   {<<"name">>,proc_lib_names(N)},
                   {<<"mq">>,  crell_web_utils:ens_bin(M)}]|R]).

proc_lib_names({M,F,Arity}) ->
    crell_web_utils:ens_bin(io_lib:format("~p:~p/~p",[M,F,Arity]));
proc_lib_names(Name) when is_atom(Name) ->
    crell_web_utils:ens_bin(atom_to_list(Name)).

mods_json([], R) ->
    jsx:encode([{<<"mods">>, lists:reverse(R)}]);
mods_json([H|T] , R) ->
    mods_json(T, [list_to_binary(atom_to_list(H))|R]).

mod_funcs_json([], R) ->
    jsx:encode([{<<"mod_funcs">>, lists:reverse(R)}]);
mod_funcs_json([{Mod,Ara}|T] , R) ->
    mod_funcs_json(T, [list_to_binary(io_lib:format("~p/~p",[Mod,Ara]))|R]).

app_env_json(AppName, AppEnv) ->
    jsx:encode([{<<"app_name">>, crell_web_utils:ens_bin(AppName)},
                {<<"app_envs">>, crell_web_utils:ens_bin(
                    io_lib:format("~p", [AppEnv])
                 )}
               ]).

db_info_json(Tables) ->
    jsx:encode([
        {<<"db_tables">>, db_info_json(Tables, [])}
    ]).

db_info_json([], R) ->
    R;
db_info_json([{ets, EtsTables} | DbTypeTables], R) ->
    % name is enough for now...
    EtsTblNames =
        lists:map(fun(TableProplist) ->
            % io:format("TBL:~p~n~n", [TableProplist]),
            {name, Name} = lists:keyfind(name, 1, TableProplist),
            Name
        end, EtsTables),
    db_info_json(DbTypeTables, [{<<"ets_tables">>, EtsTblNames} | R]);
db_info_json([{mnesia, []} | DbTypeTables], R) ->
    db_info_json(DbTypeTables, R);
db_info_json([{mnesia, MnesiaTables} | DbTypeTables], R) ->
    % name is enough for now...
    MnesiaTblNames =
        lists:map(fun(TableProplist) ->
            % io:format("TBL:~p~n~n", [TableProplist]),
            {name, Name} = lists:keyfind(name, 1, TableProplist),
            Name
        end, MnesiaTables),
    db_info_json(DbTypeTables, [{<<"mnesia_tables">>, MnesiaTblNames} | R]).

% [{ets, tables(ets)},
%  {mnesia, tables(mnesia)}
% ]

% [{ets,[[{name,mnesia_decision},
%         {id,ignore},
%         {protection,public},
%         {owner,<23945.153.0>},
%         {size,1},
%         {reg_name,mnesia_recover},
%         {type,set},
%         {keypos,2},
%         {heir,none},
%         {memory,2512},
%         {compressed,false},
%         {fixed,false}],

% [{mnesia, [[[{name,Name},
            %  {owner,Owner},
            %  {size,mnesia:table_info(Id, size)},
            %  {reg_name,RegName},
            %  {type,mnesia:table_info(Id, type)},
            %  {keypos,2},
            %  {memory,mnesia:table_info(Id, memory) * erlang:system_info(wordsize)},
            %  {storage,Storage},
            %  {index,mnesia:table_info(Id, index)}
            % ],

