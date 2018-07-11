-module(crell_nodes).
-record(?MODULE,{
    node,
    cookie,
    connected = false,
    cluster_name
}).

%% Generic
-export([
    new/2,
    create/1,
    delete/1,
    create_table/1,
    all/0,
    obj_to_proplist/1
]).

%% Specific
-export([ all_clusters/0,
          node_disconnected/1,
          node_connected/1
]).

new(Node, Cookie) ->
    GeneratedClusterName =
        integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
    new(Node, Cookie, GeneratedClusterName).

new(Node, Cookie, ClusterName) ->
    #?MODULE{
        node = Node,
        cookie = Cookie,
        cluster_name = ClusterName
    }.

create(Obj) ->
    mnesia:transaction(fun() -> mnesia:write(Obj) end).

delete(Key) ->
    mnesia:transaction(fun() -> mnesia:delete({?MODULE, Key}) end).

create_table(Nodes) ->
    try
        mnesia:table_info(?MODULE,attributes)
    catch
        exit:{aborted,{no_exists,?MODULE,attributes}} ->
            {atomic,ok} =
                mnesia:create_table(
                        ?MODULE,
                        [{type, set},
                         {disc_copies, Nodes},
                         {attributes, record_info(fields, ?MODULE)},
                         {majority, false}
                       ]);
        C:E ->
            throw({stop,[{c,C},{e,E}]})
    end.

all() ->
    {atomic, All} = mnesia:transaction(fun() ->
        all(mnesia:first(?MODULE), [])
    end),
    All.

all('$end_of_table', R) ->
    lists:reverse(R);
all(Key, R) ->
    [Entry] = mnesia:read(?MODULE, Key),
    all(mnesia:next(?MODULE, Key), [Entry|R]).

obj_to_proplist(Obj) ->
    [
        {node, Obj#?MODULE.node},
        {cookie, Obj#?MODULE.cookie}
    ].

all_clusters() ->
    lists:usort([ R#?MODULE.cluster_name || R <- all() ]).

node_disconnected(Key) ->
    mnesia:transaction(fun() ->
        [NodeRec] = mnesia:read(?MODULE, Key, write),
        ok = mnesia:write(NodeRec#?MODULE{connected = false})
    end).

node_connected(Key) ->
    mnesia:transaction(fun() ->
        [NodeRec] = mnesia:read(?MODULE, Key, write),
        ok = mnesia:write(NodeRec#?MODULE{connected = true})
    end).