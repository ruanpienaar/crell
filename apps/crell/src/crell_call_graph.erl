-module(crell_call_graph).
-export([
    run/1,
    error/0
]).
-define(XSRV,x).
-define(OPTS, [
     % {builtins, true} ,{recurse, true},
     {verbose, true},{warnings, true}
]).
-compile([{parse_transform, lager_transform}]).

run(Module) ->
    ok = initialize_xref(?XSRV),
    {ok,AllModEdges} = add_ebin_and_edges(Module),
    CallGraphBranches = edges_to_graph(AllModEdges),
    Filename = atom_to_list(Module)++".txt",
    io:format("Writing to : ~p\n",[Filename]),
    {ok,FPID} = file:open(Filename, [write, binary]),
    Data = list_to_binary(io_lib:format("~p\n",[CallGraphBranches])),
    ok = file:write(FPID,Data),
    ok = file:close(FPID),
    stop_xref(?XSRV).

edges_to_graph(L) when is_list(L) ->
    edges_to_graph(L, []).

edges_to_graph([],R) ->
    lists:reverse(R);
edges_to_graph([H|T], R) ->
    edges_to_graph(T, [edge_to_graph(H)|R]).

edge_to_graph({From, To}) ->
    {ModA, FuncA, ArityA} = From,
    {ModB, FuncB, ArityB} = To,
    {
        {{ModA, FuncA, ArityA},{ModB, FuncB, ArityB}},
        next_edge_to_graph(ModB, FuncB, ArityB)
    }.

next_edge_to_graph(Module, Func, Arity) ->
    {ok,AllModEdges} = add_ebin_and_edges(Module),
    [_,NextModEdges] =
        lists:foldl(fun 
                        %% exclude calls, that call themselves, like list loops
                        (Edge={{M, F, A}, {M, F, A}}, 
                                [[{M, F, A}, _PreviousEdge], Acc]) ->
                            io:format("L) Edge ~p\n", [Edge]), 
                            [[{M, F, A}, Edge], Acc];
                        %% Exclude calls that have circular calls ( infinite loops )
                        (Edge={{M, F, A}, {M2, F2, A2}}, 
                                [[{M, F, A}, {{M, F, A}, {M2, F2, A2}}], Acc]) ->
                            io:format("C) Edge ~p\n", [Edge]), 
                            [[{M, F, A}, Edge], Acc];
                        %% Exclude calls that have circular calls ( infinite loops )
                        (Edge={{M, F, A}, {M2, F2, A2}}, 
                                [[{M, F, A}, {{M2, F2, A2}, {M, F, A}}], Acc]) ->
                            io:format("C) Edge ~p\n", [Edge]), 
                            [[{M, F, A}, Edge], Acc];
                        %% Using the edge, we're interested in.
                        (Edge={{M, F, A}, {_,_,_}}, 
                                [[{M, F, A}, _PreviousEdge], Acc]) ->
                            io:format("Y) Edge ~p\n", [Edge]),
                            [[{M, F, A}, Edge], [Edge|Acc]];
                        %% Ignoring non-interesting edge
                        (Edge, 
                                [[{M, F, A}, _PreviousEdge], Acc]) ->
                            % io:format("N) Edge ~p\n", [Edge]),
                            [[{M, F, A}, Edge], Acc]
        end, [[{Module,Func,Arity}, undefined],[]], AllModEdges),
    lists:map(fun(Edge) -> 
        edge_to_graph(Edge)
    end, lists:reverse(NextModEdges)).

add_ebin_and_edges('$M_EXPR') ->
    %% Still have to implement Unresolved calls, 
    %% maybe actually read the source code, and try to see what the values we're.........
    {ok,[]};
add_ebin_and_edges(Module) ->
    %% io:format("Looking for module :~p\n\n",[Module]),
    case crell_mods:find(Module) of
        {file, preloaded} ->
            {ok,_Edges} = module_edges(?XSRV, Module),
            {ok,[]};
        {file, ModAbsPath} ->
            AbsEbin = string:left(ModAbsPath, abs(length(ModAbsPath) - length(atom_to_list(Module)++".beam"))),
            _ModulesOrThrow = add_ebin(AbsEbin),
            {ok,_Edges} = module_edges(?XSRV, Module)
    end.


initialize_xref(Name) ->
    case xref:start(Name, ?OPTS) of
        {error, {already_started, _}} ->
            stop_xref(Name),
            xref:start(Name);
        {ok, _Ref} ->
            ok
    end,
    ok = xref:set_default(Name, ?OPTS).

stop_xref(Ref) ->
    xref:stop(Ref),
    ok.

add_ebin(EbinPath) ->
    try
        {ok,_Modules} = xref:add_directory(?XSRV, EbinPath, ?OPTS)
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("~p got ~p, ~p\n~p\n",[C,E,ST])
    end.

module_edges(XServ, Module) ->
    try
        QueryStr = lists:flatten( io_lib:format("E | ['~p']",[Module]) ),
        {ok,Edges} = xref:q(XServ, QueryStr)
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("~p got ~p, ~p\n~p\n",[C,E,ST]),
            {error,[{c,C},{e,E},{stacktrace,ST}]}
    end.

error() ->
	lager:error("Test").