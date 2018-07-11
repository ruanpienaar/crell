-module(crell_server_tests).
-include_lib("eunit/include/eunit.hrl").

crell_server_unit_test_() ->
    {foreach,
     % Setup Fixture
     fun() ->
        {ok, _} = erlang_testing:start_distrib(?MODULE),
        {ok, Host} = inet:gethostname(),
        [Node] = erlang_testing:slaves_setup([{Host}]),
        stopped = mnesia:stop(),
        ok = mnesia:delete_schema([node()]),
        ok = mnesia:create_schema([node()]),
        ok = mnesia:start(),
        {atomic,ok} = crell_nodes:create_table([node()]),
        {ok, Pid} = crell_server:start_link(),
        Pid
     end,
     % Cleanup Fixture
     fun(Pid) ->
         true = erlang:unlink(Pid),
         erlang:exit(Pid, kill),
         stopped = mnesia:stop(),
         application:stop(mnesia)
     end,
     % List of tests
     [
       % Example test
       {"crell_server:toggle_tracing/0",
            ?_assert(unit_testing:try_test_fun(fun toggle_tracing/0))}
     ]
    }.

toggle_tracing() ->
    ?assertEqual(
        true,
        crell_server:toggle_tracing()
    ),
    ?assertEqual(
        false,
        crell_server:toggle_tracing()
    ).
