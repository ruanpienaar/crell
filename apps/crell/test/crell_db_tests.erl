-module(crell_db_tests).

-include_lib("eunit/include/eunit.hrl").

crell_db_unit_test_() ->
    {foreach,
     fun() ->
        tbl = ets:new(tbl, [public, ordered_set, named_table]),
        [ true = ets:insert(tbl, {I, undefined}) || I <- lists:seq(1, 1000) ],
        % {ok, _} = dbg:tracer(),
        % {ok, _} = dbg:p(all, call),
        % {ok, _} = dbg:tpl(crell_db, all_ets_keys_loop, cx),
        setup_response
     end,
     fun(setup_response) ->
        ok
     end,
     [
        fun ets_entries_per_page/0,
        fun all_ets_keys/0
     ]
    }.

ets_entries_per_page() ->
    ?assertEqual(
        {[{1,undefined},
          {2,undefined},
          {3,undefined},
          {4,undefined},
          {5,undefined},
          {6,undefined},
          {7,undefined},
          {8,undefined},
          {9,undefined},
          {10,undefined},
          {11,undefined},
          {12,undefined},
          {13,undefined},
          {14,undefined},
          {15,undefined},
          {16,undefined},
          {17,undefined},
          {18,undefined},
          {19,undefined},
          {20,undefined},
          {21,undefined},
          {22,undefined},
          {23,undefined},
          {24,undefined},
          {25,undefined},
          {26,undefined},
          {27,undefined},
          {28,undefined},
          {29,undefined},
          {30,undefined},
          {31,undefined},
          {32,undefined},
          {33,undefined},
          {34,undefined},
          {35,undefined},
          {36,undefined},
          {37,undefined},
          {38,undefined},
          {39,undefined},
          {40,undefined},
          {41,undefined},
          {42,undefined},
          {43,undefined},
          {44,undefined},
          {45,undefined},
          {46,undefined},
          {47,undefined},
          {48,undefined},
          {49,undefined},
          {50,undefined}],
         1000},
        crell_db:ets_entries_per_page(node(), tbl, 1)
    ),
        ?assertEqual(
        {[{51,undefined},
          {52,undefined},
          {53,undefined},
          {54,undefined},
          {55,undefined},
          {56,undefined},
          {57,undefined},
          {58,undefined},
          {59,undefined},
          {60,undefined},
          {61,undefined},
          {62,undefined},
          {63,undefined},
          {64,undefined},
          {65,undefined},
          {66,undefined},
          {67,undefined},
          {68,undefined},
          {69,undefined},
          {70,undefined},
          {71,undefined},
          {72,undefined},
          {73,undefined},
          {74,undefined},
          {75,undefined},
          {76,undefined},
          {77,undefined},
          {78,undefined},
          {79,undefined},
          {80,undefined},
          {81,undefined},
          {82,undefined},
          {83,undefined},
          {84,undefined},
          {85,undefined},
          {86,undefined},
          {87,undefined},
          {88,undefined},
          {89,undefined},
          {90,undefined},
          {91,undefined},
          {92,undefined},
          {93,undefined},
          {94,undefined},
          {95,undefined},
          {96,undefined},
          {97,undefined},
          {98,undefined},
          {99,undefined},
          {100,undefined}],
         1000
        },
        crell_db:all_ets_keys(node(), tbl, 2)
    ).


all_ets_keys() ->
    % page 1 - first 50
    ?assertEqual(
        {51, 1000,
         [{1,undefined},
          {2,undefined},
          {3,undefined},
          {4,undefined},
          {5,undefined},
          {6,undefined},
          {7,undefined},
          {8,undefined},
          {9,undefined},
          {10,undefined},
          {11,undefined},
          {12,undefined},
          {13,undefined},
          {14,undefined},
          {15,undefined},
          {16,undefined},
          {17,undefined},
          {18,undefined},
          {19,undefined},
          {20,undefined},
          {21,undefined},
          {22,undefined},
          {23,undefined},
          {24,undefined},
          {25,undefined},
          {26,undefined},
          {27,undefined},
          {28,undefined},
          {29,undefined},
          {30,undefined},
          {31,undefined},
          {32,undefined},
          {33,undefined},
          {34,undefined},
          {35,undefined},
          {36,undefined},
          {37,undefined},
          {38,undefined},
          {39,undefined},
          {40,undefined},
          {41,undefined},
          {42,undefined},
          {43,undefined},
          {44,undefined},
          {45,undefined},
          {46,undefined},
          {47,undefined},
          {48,undefined},
          {49,undefined},
          {50,undefined}]},
        crell_db:all_ets_keys(node(), tbl, undefined)
    ),

    % % But do not pass in cursor key - we need cursor key on subseuent calls
    % ?assertException(
    %     error,
    %     function_clause,
    %     crell_db:all_ets_keys(node(), tbl, 2)
    % ),

    % pass in the cursor key from the previous calls

    ?assertEqual(
        {101, 1000,
         [{51,undefined},
          {52,undefined},
          {53,undefined},
          {54,undefined},
          {55,undefined},
          {56,undefined},
          {57,undefined},
          {58,undefined},
          {59,undefined},
          {60,undefined},
          {61,undefined},
          {62,undefined},
          {63,undefined},
          {64,undefined},
          {65,undefined},
          {66,undefined},
          {67,undefined},
          {68,undefined},
          {69,undefined},
          {70,undefined},
          {71,undefined},
          {72,undefined},
          {73,undefined},
          {74,undefined},
          {75,undefined},
          {76,undefined},
          {77,undefined},
          {78,undefined},
          {79,undefined},
          {80,undefined},
          {81,undefined},
          {82,undefined},
          {83,undefined},
          {84,undefined},
          {85,undefined},
          {86,undefined},
          {87,undefined},
          {88,undefined},
          {89,undefined},
          {90,undefined},
          {91,undefined},
          {92,undefined},
          {93,undefined},
          {94,undefined},
          {95,undefined},
          {96,undefined},
          {97,undefined},
          {98,undefined},
          {99,undefined},
          {100,undefined}]},
        crell_db:all_ets_keys(node(), tbl, 51)
    ).

crell_db_ets_entries_per_page_non_orderedset_unit_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [
        fun crell_db_ets_entries_per_page_set/0,
        fun crell_db_ets_entries_per_page_bag/0
     ]
    }.

crell_db_ets_entries_per_page_set() ->
    ok.

crell_db_ets_entries_per_page_bag() ->
    ok.