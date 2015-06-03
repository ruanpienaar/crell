-module(crell_tests).
-include_lib("eunit/include/eunit.hrl").

% -----------------------------------------------------

crell_test_() ->
	{setup,
		fun setup/0,
		fun cleanup/1,
		[
			{"all_mods",				fun crell_mods_all/0},
			{"find module",				fun crell_mods_find/0},
			{"module_edges",			fun crell_mods_module_edges/0}
		]
	}.

% -----------------------------------------------------

setup() ->

	%% MAybe add the dir here.........

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
	{file, AbsPath} = crell_mods:find(lists),
	?assert(is_list(AbsPath)),
	?assert(string:str(AbsPath, "lists.beam") =/= 0).

crell_mods_module_edges() ->
	Name=x,
	ok = crell_mods:initialize_xref(Name, [{builtins, true}]),
	{ok, Modules} = crell_mods:add_dir_xref(Name, "."),
	io:format("Modules : ~p\n\n",[Modules]),
	?assert(lists:member(mod_a, Modules) and
			lists:member(mod_b, Modules) and
			lists:member(mod_c, Modules) and
			lists:member(mod_d, Modules)),
	{ok,[{{mod_a,func_b,0},{mod_b,func_b,0}},
         {{mod_a,func_c,0},{mod_b,func_b,0}},
         {{mod_a,func_c,0},{mod_c,func_c,0}},
         {{mod_a,func_d,0},{mod_b,func_b,0}},
         {{mod_a,func_d,0},{mod_c,func_c,0}},
         {{mod_a,func_d,0},{mod_d,func_d,0}},
         {{mod_a,func_e,0},{erlang,now,0}},
         {{mod_a,func_e,0},{mod_b,func_b,0}},
         {{mod_a,func_f,0},{mod_a,func_b,0}}]} = crell_mods:module_edges(Name, mod_a).