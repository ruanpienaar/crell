-module (crell_trace_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1,
         start_switch/0,
         stop_switch/0,
         children/0
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
    C = crell_trace,
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
          [{trace_id,{C, start_link, []}, permanent, 1000, worker, [C]}
          ]}
    }.

start_switch() ->
    [ start_child(N) || N <- [ 'test1@rpmbp.local', 'test2@rpmbp.local' ]].

stop_switch() ->
    [ erlang:exit(whereis(crell_trace:name(N)), normal)
    || N <- [ 'test1@rpmbp.local', 'test2@rpmbp.local' ]].

start_child(Node) ->
    supervisor:start_child(?MODULE, [Node]).

children() ->
    supervisor:which_children(?MODULE).

