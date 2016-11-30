-module(crell_ws_api).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-define(STATE, crell_ws_api).
-record(?STATE, { }).

init(Req, Opts) ->
    erlang:register(crell_goanna_api_ws, self()),
    process_flag(trap_exit, true),
    % io:format("Opts : ~p~n", [Opts]),
    % erlang:send_after(1000, self(), bla),
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
            NodesJson = nodes_json(),
            erlang:send_after(1000, self(), nodes),
            {reply, {text, NodesJson}, Req, State};
        UnknownJson ->
            io:format("UnknownJson: ~p~n", [UnknownJson]),
            Json = jsx:encode([{<<"unknown_json">>, UnknownJson}]),
            {reply, {text, Json}, Req, State}
    end.

websocket_info(nodes, Req, State) ->
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
    % [{stdlib,"ERTS  CXC 138 10","2.6"},
    %  {kernel,"ERTS  CXC 138 10","4.1"}]
    NodeAppsJson = [ [{<<"name">>, crell_web_utils:ens_bin(AppName)}
                    , {<<"erts_vsn">>, crell_web_utils:ens_bin(ErtsVsn)}
                    , {<<"vsn">>, crell_web_utils:ens_bin(Vsn)}
                    ] || {AppName, ErtsVsn, Vsn} <- NodeApps],
    jsx:encode([{<<"node_apps">>, NodeAppsJson}]).
