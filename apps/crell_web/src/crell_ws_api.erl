-module(crell_ws_api).

-include_lib("kernel/include/logger.hrl").

-export([
    init/2,
    websocket_handle/2,
    websocket_info/2
]).

-define(STATE, crell_ws_api).
-record(?STATE, {}).

init(Req, _Opts) ->
    true = crell_notify:subscribe({node_events}),
    % process_flag(trap_exit, true),
    {cowboy_websocket, Req, #?STATE{}}.

websocket_handle({text, ReqJson}, State) ->
    %% TODO: check for MFA, then just call apply?
    %% TODO: change to maps
    case jsx:decode(ReqJson, [{return_maps, false}]) of
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"nodes">>},
         {<<"args">>,[]}] ->
            NodesJson = nodes_json(),
            {reply, {text, NodesJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"connecting_nodes">>},
         {<<"args">>,[]}] ->
            ConnectingNodesJson = connecting_nodes_json(),
            {reply, {text, ConnectingNodesJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"remote_which_applications">>},
         {<<"args">>,[Node]}] ->
            NodeAppsJson = get_node_apps_json(Node),
            {reply, {text, NodeAppsJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"add_node">>},
         {<<"args">>,[Node, Cookie]}] ->
            ok = crell_server:add_node(crell_web_utils:ens_atom(Node),
                                       crell_web_utils:ens_atom(Cookie)),
            ConnectingNodesJson = connecting_nodes_json(),
            {reply, {text, ConnectingNodesJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,DeleteFunc},
         {<<"args">>,Nodes}] when DeleteFunc == <<"del_node">> orelse
                                  DeleteFunc == <<"conn_del_node">> ->
            ok = lists:foreach(fun(Node) ->
                ok = crell_server:remove_node(
                    crell_web_utils:ens_atom(Node)
                )
            end, Nodes),
            {reply, reply_ok(), State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"non_sys_processes">>},
         {<<"args">>,[Node]}] ->
            PidsJson = pids_json(crell_server:non_sys_processes(
                crell_web_utils:ens_atom(Node))
            ),
            {reply, {text, PidsJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"runtime_modules">>},
         {<<"args">>,[Node]}] ->
            Mods = crell_server:runtime_modules(
                crell_web_utils:ens_atom(Node)
            ),
            ModsJson = mods_json(Mods, []),
            % io:format("Mods Json ~p\n", [ModsJson]),
            {reply, {text, ModsJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"is_tracing">>},
         {<<"args">>,[]}] ->
            {reply, reply("is_tracing", crell_server:is_tracing()), State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"toggle_tracing">>},
         {<<"args">>,[Node]}] ->
            {reply, reply("is_tracing", crell_server:toggle_tracing(Node)), State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"cluster_modules">>},
         {<<"args">>,[]}] ->
            Mods = crell_server:cluster_modules(),
            ModsJson = mods_json(Mods, []),
            {reply, {text, ModsJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"cluster_module_functions">>},
         {<<"args">>,[Mod]}] ->
            ModsFuncs = crell_server:cluster_module_functions(
                crell_web_utils:ens_atom(Mod)
            ),
            ModsFuncJson = mod_funcs_json(ModsFuncs, []),
            {reply, {text, ModsFuncJson}, State};
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
            {reply, {text, AppEnvJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"get_db_tables">>},
         {<<"args">>,[Node]}] ->
            DbTypeTables =
                crell_server:get_db_tables(
                    crell_web_utils:ens_atom(Node)
                ),
            {reply, {text, db_info_json(DbTypeTables)}, State};
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
            {reply, {text, DownloadJson}, State};
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
            {reply, {text, DownloadJson}, State};
    	[{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"cluster_application_consistency">>},
         {<<"args">>,[]}] ->
            {reply, reply('cluster_application_consistency',
                         crell_server:cluster_application_consistency()),
                State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"recon_inj_status">>},
         {<<"args">>,[]}] ->
            {reply, reply('recon_inj_status',
                          crell_server:recon_inj_status()),
                State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"discover_neighbour_nodes">>},
         {<<"args">>,Nodes}] ->
            ok = crell_server:discover_neighbour_nodes(
                [ crell_web_utils:ens_atom(Node) || Node <- Nodes ]
            ),
            NodesJson = nodes_json(),
            {reply, {text, NodesJson}, State};
        [{<<"module">>,<<"crell_db">>},
         {<<"function">>,<<"ets_entries_per_page">>},
         {<<"args">>,[Node, Tbl, PageNmr]}] ->
            KeysJson =
                ets_record_json(
                    crell_db:ets_entries_per_page(
                        crell_web_utils:ens_atom(Node),
                        crell_web_utils:ens_atom(Tbl),
                        crell_web_utils:ens_int(PageNmr)
                    ),
                    PageNmr
                ),
            {reply, {text, KeysJson}, State};
        [{<<"module">>,<<"crell_db">>},
         {<<"function">>,<<"mnesia_entries_per_page">>},
         {<<"args">>,[Node, Tbl, PageNmr]}] ->
            KeysJson =
                mnesia_record_json(
                    crell_db:mnesia_entries_per_page(
                        crell_web_utils:ens_atom(Node),
                        crell_web_utils:ens_atom(Tbl),
                        crell_web_utils:ens_int(PageNmr)
                    ),
                    PageNmr
                ),
            {reply, {text, KeysJson}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"module_source">>},
         {<<"args">>,[Node, Module]}] ->
            {ok, Source} = crell_server:module_source(
                crell_web_utils:ens_atom(Node),
                crell_web_utils:ens_atom(Module)
            ),
            % logger:info(#{source => Source}),
            % Json = <<"ok">>,
            Json = jsx:encode(#{code => Source}),
            % logger:info(#{json => Json}),
            {reply, {text, Json}, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"get_remote_pid_info">>},
         {<<"args">>,[Node, PidStr]}] ->
            Pid = list_to_pid(binary_to_list(PidStr)),
            {ok, Info} = crell_server:remote_pid_info(
                crell_web_utils:ens_atom(Node),
                Pid
            ),
            Json = remote_pid_info_json(Pid, Info),
            {reply, {text, Json}, State};
        UnknownJson ->
            logger:error(#{ unknown_json => UnknownJson }),
            Json = jsx:encode([{<<"unknown_json">>, UnknownJson}]),
            {reply, {text, Json}, State}
    end.

reply_ok() ->
    reply(reply, ok).

reply(Reply, Val) ->
    {text, jsx:encode([{crell_web_utils:ens_bin(Reply),
                        crell_web_utils:ens_bin(Val)}])}.

% websocket_info(nodes, Req, State) ->
%     io:format("Websocket info nodes\n"),
%     NodesJson = nodes_json(),
%     {reply, {text, NodesJson}, State};
websocket_info({crell_notify,
                {node_events},
                {node_connecting, Node}}, State) ->
    {reply, {text, jsx:encode([{<<"node_connecting">>,
                                crell_web_utils:ens_bin(Node)}])}, State};
websocket_info({crell_notify,
                {node_events},
                {node_deleted, Node}}, State) ->
    {reply, {text, jsx:encode([{<<"node_deleted">>,
                                crell_web_utils:ens_bin(Node)}])}, State};
websocket_info({crell_notify,
                {node_events},
                {Event,Node,_Cookie}
               }, State) when Event == node_connected;
                                   Event == node_disconnected ->
    {reply, {text, jsx:encode([{crell_web_utils:ens_bin(Event),
                                crell_web_utils:ens_bin(Node)}])}, State};
websocket_info({timeout, _Ref, <<"nodes">>}, State) ->
    NodesJson = nodes_json(),
    {reply, {text, NodesJson}, State};
websocket_info(Info, State) ->
    % io:format("Info : ~p\n", [Info]),
    logger:notice(#{websocket_info => Info}),
    Json = <<"reply">>,
    {reply, {text, Json}, State}.

nodes_json() ->
    jsx:encode([{<<"nodes">>,
        [ Node || Node <- crell_server:nodes() ]}]).

connecting_nodes_json() ->
    jsx:encode([{<<"connecting_nodes">>,
        [ Node || Node <- crell_server:connecting_nodes() ]}]).

get_node_apps_json(Node) ->
    NodeApps = crell_server:remote_which_applications(crell_web_utils:ens_atom(Node)),
    NodeAppsJson = [ [{<<"name">>, crell_web_utils:ens_bin(AppName)}
                    , {<<"erts_vsn">>, crell_web_utils:ens_bin(Desc)}
                    , {<<"vsn">>, crell_web_utils:ens_bin(Vsn)}
                    ] || {AppName, Desc, Vsn} <- NodeApps],
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
    % Mods2 = lists:map(fun({Module, Opts}) ->
    %     io:format("----------------------\n"),
    %     io:format("~p", [{Module, Opts}]),
    %     io:format("~p", [{Module, recusrive_map_to_list(Opts)}]),
    %     _ = jsx:encode({Module, recusrive_map_to_list(Opts)}),
    %     io:format("----------------------\n"),
    %     {Module, Opts}
    % end, R),
    jsx:encode([{<<"mods">>, R}]);
mods_json([H|T] , R) when is_atom(H) ->
    mods_json(T, [list_to_binary(atom_to_list(H))|R]);
mods_json([{H, Info}|T] , R) when is_atom(H) ->
    mods_json(T, [{atom_to_binary(H), binary:list_to_bin(io_lib:format("~p", [Info]))} | R]).

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
            {size, Size} = lists:keyfind(size, 1, TableProplist),
            [{<<"name">>, Name},
             {<<"size">>, Size}
            ]
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
            {size, Size} = lists:keyfind(size, 1, TableProplist),
            [{<<"name">>, Name},
             {<<"size">>, Size}
            ]
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

%% TODO: unify this shity copy paste effort

ets_record_json({RecordCount, Records}, PageNmr) ->
    ets_record_json(RecordCount, Records, [], PageNmr).

ets_record_json(RecordCount, [], R, PageNmr) ->
    L = lists:reverse(R),
    jsx:encode(
        [ {<<"ets_records">>,
            [{<<"count">>, crell_web_utils:ens_bin(RecordCount)},
             {<<"records">>, L}
            ]
          },
          {<<"PageNmr">>, crell_web_utils:ens_bin(PageNmr)}
        ]
    );
ets_record_json(RecordCount, [H|T], R, PageNmr) ->
    ets_record_json(RecordCount, T, [ crell_web_utils:ens_bin(H) | R ], PageNmr).


mnesia_record_json({badrpc, Reason}, _) ->
    jsx:encode([
        {<<"mnesia_records_error">>, crell_web_utils:ens_bin(Reason)}
    ]);
mnesia_record_json({RecordCount, Records}, PageNmr) ->
    mnesia_record_json(RecordCount, Records, [], PageNmr).

mnesia_record_json(RecordCount, [], R, PageNmr) ->
    L = lists:reverse(R),
    jsx:encode(
        [ {<<"mnesia_records">>,
            [{<<"count">>, crell_web_utils:ens_bin(RecordCount)},
             {<<"records">>, L}
            ]
          },
          {<<"PageNmr">>, crell_web_utils:ens_bin(PageNmr)}
        ]
    );
mnesia_record_json(RecordCount, [H|T], R, PageNmr) ->
    mnesia_record_json(RecordCount, T, [ crell_web_utils:ens_bin(H) | R ], PageNmr).

remote_pid_info_json(Pid, Info) ->
    Data = #{
        remote_pid_info => #{
            pid => crell_web_utils:jsx_safe_string(Pid),
            info => crell_web_utils:map_from_proplist_r(Info)
        }
    },
    io:format("Data ~p\n", [Data]),
    jsx:encode(Data).