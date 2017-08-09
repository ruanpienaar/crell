-module(crell_ws_api).

-compile(export_all).

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
         {<<"function">>,<<"remote_which_applications">>},
         {<<"args">>,[Node]}] ->
            NodeAppsJson = get_node_apps_json(Node),
            {reply, {text, NodeAppsJson}, Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"add_node">>},
         {<<"args">>,[Node, Cookie]}] ->
            ok = crell_server:add_node(crell_web_utils:ens_atom(Node),
                                       crell_web_utils:ens_atom(Cookie)),
            %% TODO: improve this sleep nonsense...
            %% Hawk is currently configured to wait 1000ms ( 500ms sleep, 10 attempts )
            erlang:start_timer(1050, self(), <<"nodes">>),
            {reply, reply_ok(), Req, State};
        [{<<"module">>,<<"crell_server">>},
         {<<"function">>,<<"del_node">>},
         {<<"args">>,[Node]}] ->
            ok = crell_server:remove_node(
                crell_web_utils:ens_atom(Node)
            ),
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
         {<<"function">>,<<"cluster_application_consistency">>},
         {<<"args">>,[]}] ->
            _ConsistencyResponse = crell_server:cluster_application_consistency(),
            _ConsistencyResponseJson  = undefined,
            {reply, {text, <<"{}">>}, Req, State};
        UnknownJson ->
            io:format("[~p] UnknownJson: ~p~n", [?MODULE,UnknownJson]),
            Json = jsx:encode([{<<"unknown_json">>, UnknownJson}]),
            {reply, {text, Json}, Req, State}
    end.

reply(Reply, Val) ->
    {text, jsx:encode([{crell_web_utils:ens_bin(Reply),
                        crell_web_utils:ens_bin(Val)}])}.

reply_ok() ->
    reply(reply, ok).

% websocket_info(nodes, Req, State) ->
%     io:format("Websocket info nodes\n"),
%     NodesJson = nodes_json(),
%     {reply, {text, NodesJson}, Req, State};
websocket_info({crell_notify,
                {node_events},
                {node_connecting,Node}}, Req, State) ->
    {reply, {text, jsx:encode([{<<"node_connecting">>,
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