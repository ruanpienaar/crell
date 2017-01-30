-module(crell_utils).
-export([
    test_1/0,
    test_2/0,
    test_3/0
]).

test_1() ->
   	io:format("Test").

test_2() ->
    test_3().

test_3() ->
    test_1().