-module(crell_mods).
-export([
    all/0,
    find/1,
    follow/4,
    add_all_ebins/1
    ]).

-define(OPTS,   [{builtins, true},{recurse, true},{verbose, true},{warnings, true}] ).

-spec all() -> 
    [{Module :: module(), AbsolutePath :: string()}].
all() ->
    %% Maybe cache all the modules....
    _Loaded = code:all_loaded().

-spec find(Module :: module()) -> 
    {Module :: module(), AbsolutePath :: string()}.
find(Module) ->
    code:is_loaded(Module).

-spec follow(Module :: module(), Function :: atom(), Arity :: non_neg_integer(), ModuleAbsPath :: string()) ->
    Stuff :: any() | {error, module_not_found}. %% Define this .......
follow(Module, Function, Arity, ModuleAbsPath) ->
    XServ=?MODULE,
    try
        initialize_xref(XServ, _Options=?OPTS),
        % ok(xref:add_directory(XServ, ModuleAbsPath)),

        case find(Module) of 
            {file,_AbsPathAndFileName} ->
                module_edges(XServ, Module);
            false ->
                {error, module_not_found}
        end

    catch
        C:E ->
            lager:error("~p got ~p, ~p\n~p\n",[C,E,erlang:get_stacktrace()])
    end.

%% Interal: --------------------------------------------------------------------------

initialize_xref(Name, Options) ->
    case xref:start(Name) of
        {error, {already_started, _}} ->
            stop_xref(Name),
            xref:start(Name);
        {ok, _Ref} ->
            ok
    end,
    % XRefOpts = [{verbose, proplists:is_defined(verbose, Options)},
    %             {warnings, proplists:is_defined(warnings, Options)}
    %             ],

    XRefOpts = [{verbose, true},{warnings, true}],

    ok = xref:set_default(Name, XRefOpts).

stop_xref(Ref) ->
    xref:stop(Ref),
    ok.


ok({ok, Result}) -> Result;
ok(Error)        -> throw(Error).


module_edges(XServ, Module) ->
    modules_edges(XServ, [Module]).

modules_edges(XServ, Modules) ->
    %% Query = "ME ||| [" ++ string:join(["'" ++ atom_to_list(M) ++ "'" || M <- Modules], ",") ++ "]",
    Query = "F ||| [" ++ string:join(["'" ++ atom_to_list(M) ++ "'" || M <- Modules], ",") ++ "]",
    {ok, Results} = xref:q(XServ, Query).

add_all_ebins(XServ) ->
    lists:foldl(fun({Mod,preloaded},Acc)   -> 
                        Acc;
                   ({Mod, ModAbsPath},Acc) -> 
                        P = string:left(ModAbsPath, abs(length(ModAbsPath) - length(atom_to_list(Mod)++".beam"))),
                        ok(xref:add_directory(XServ, P)),
                        [P|Acc]
    end, [], all()).














% Erlang and Erlang OTP
% Erlang/OTP/Mnesia design considerations and limitations
% Erlang profiling, tracing and debug tools (cprof, fprof, etop, trace, redbug,
% dbg)
% Erlang Crash File Analysis (DTrace, SystemTap)
% Detailed understanding of the Erlang VM
% Erlang runtime system schedulers
% Erlang runtime system configuration and optimisation relating to Oracle
% Sparc and Intel x86 CPU system architectures
% Erlang Beam optimisations through compile time options and vendor specific
% options
% XML and associated processing technologies
% SCM
% Appropriate use of industry design patterns
% Good experience and good knowledge of :
% Defect management tools
% Ability to design systems with approaches to handling state,
% persistent data, and data replication in scalable, distributed multicore
% systems.
% Ability to analyse and solve the highest priority/severity defects, and
% most technically challenging problems to a successful outcome.
% Able to correctly implement the most complex functionality, and
% create frameworks to a high standard.
% Excellent knowledge and experience of UML and Object-Orientated
% Analysis and Design principles.
% In depth experience in the full lifecycle of software development
% (inception to transition/Live)
% Ability to provide technical leadership to a team to deliver to agreed
% deadlines. Able to competently carry out the most complex tasks
% without support when minimal instructions are given, even when
% outside influences reduce the time available
% Proven experience in creating and reviewing technical/design
% documentation.
% Proven track record in designing scalable, high performance
% processing systems. Highly experienced technical leadership in the
% implementation of mission critical, distributed, concurrent, multicore
% processing systems.
% Able to enter in to debates with architects to identify optimum
% solutions which meet both the technogical and business needs. Is
% able to communicate and convince their audience and lead
% discussions and workshops to an agreed solution.
% Able to communicate at senior/management levels across the
% company i.e. business, programme management, analysis, testing,
% bid management.
% Able to quickly grasp all new technical and business knowledge to
% such an extent that they can almost immediately apply these new
% concepts in a work context and work independently and confidently
% with them.