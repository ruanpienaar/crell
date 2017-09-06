-module(crell_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 60000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, P} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

    % % %% add the old nodes:
    % lists:foreach(fun(NodeRec) ->
    %     NP = crell_nodes:obj_to_proplist(NodeRec),
    %     {node, Node} = lists:keyfind(node, 1, NP),
    %     {cookie, Cookie} = lists:keyfind(cookie, 1, NP),
    %     ok = crell_server:add_node(Node, Cookie)
    % end, crell_nodes:all()),

    {ok, P}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(crell_server, worker)]} }.

