-module(crell_tests).
-include_lib("eunit/include/eunit.hrl").

% -----------------------------------------------------

crell_test_() ->
	{setup,
		fun setup/0,
		fun cleanup/1,
		[
			{"all_mods",				fun crell_mods_all/0},
			{"find module",				fun crell_mods_find/0}.
			{"Get module's Call Graph", fun crell_mods_follow/0}
		]
	}.

% -----------------------------------------------------

setup() ->
	ok.

cleanup(_Pid) ->
	ok.

% -----------------------------------------------------

crell_mods_all() ->
	All = crell_mods:all(),
	?assert(is_list(All)),
	?assert({erlang, preloaded} == lists:keyfind(erlang, 1, All)),
	{lists,AbsPath} = lists:keyfind(lists, 1, All),
	?assert(is_list(AbsPath)).

crell_mods_find() ->
	{lists, AbsPath} = crell_mods:find(lists),
	?assert(is_list(AbsPath)).

crell_mods_follow() ->
	true.

