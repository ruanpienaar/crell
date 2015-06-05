-module(crell_utils).
-export([
    test_1/0,
    test_2/0,
    test_3/0
]).

test_1() ->
   	lager:info("Test").

test_2() ->
    test_3().

test_3() ->
    test_1().