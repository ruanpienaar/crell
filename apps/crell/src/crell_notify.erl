-module(crell_notify).

-export([action/1,
         subscribe/1
]).

%% ----------

action({node_connected, Node, Cookie}) ->
    publish({node_events}, {node_connected, Node, Cookie});
action({node_disconnected, Node, Cookie}) ->
    publish({node_events}, {node_disconnected, Node, Cookie});
action({node_connecting, Node}) ->
    publish({node_events}, {node_connecting, Node}).

%% ----------

subscribe(Event) when Event == {node_events} ->
    gproc:reg({p, l, {?MODULE, Event}}).

publish(Event, Data) when Event == {node_events} ->
    gproc:send({p, l, {?MODULE, Event}}, {?MODULE, Event, Data}).

%% ----------