-module(crell_mods).

-include_lib("kernel/include/logger.hrl").

-export([
    all/0,
    find/1,
    module_edges/2,

    %% Xref:
    initialize_xref/2,
    add_dir_xref/2
]).

-define(OPTS, [{builtins, true}
     % ,{recurse, true},{verbose, true},{warnings, true}
]).

-spec all() ->
    [{Module :: module(), AbsolutePath :: string()}].
all() ->
    %% Maybe cache all the modules....
    _Loaded = code:all_loaded().

-spec find(Module :: module()) ->
    {Module :: module(), AbsolutePath :: string()}.
find(Module) ->
    code:is_loaded(Module).

module_edges(XServ, Module) ->
    try
        QueryStr = lists:flatten( io_lib:format("E | ['~p']",[Module]) ),
        {ok,Edges} = xref:q(XServ, QueryStr)
    catch
        C:E:S ->
            logger:error(#{ c => C, e => E, s => S}),
            {error,[{c,C},{e,E},{stacktrace,S}]}
    end.

initialize_xref(Name, Options) ->
    case xref:start(Name) of
        {error, {already_started, _}} ->
            stop_xref(Name),
            xref:start(Name);
        {ok, _Ref} ->
            ok
    end,
    XRefOpts = [{builtins, true}], %% {verbose, true}, {warnings, true}
    ok = xref:set_default(Name, XRefOpts).

add_dir_xref(XServ, EbinPath) ->
    xref:add_directory(XServ, EbinPath, ?OPTS).

stop_xref(Ref) ->
    xref:stop(Ref),
    ok.

ok({ok, Result}) -> Result;
ok(Error)        -> throw(Error).

-spec add_all_ebins(any()) ->
    [{Dir :: string(), [atom()] }].
add_all_ebins(XServ) ->
    lists:foldl(fun({Mod,preloaded}, Acc) ->
                        Acc;
                      ({Mod, ModAbsPath}, Acc) ->
                        EbinPath = beam_dir(ModAbsPath, Mod),
                        Mods = add_dir_xref(XServ, EbinPath),
                        [{EbinPath, Mods} | Acc]
    end, [], all()).

% in:   /dir/file.beam
% out:  /dir/
beam_dir(ModAbsPath, Mod) ->
    string:left(ModAbsPath, abs(length(ModAbsPath) - length(atom_to_list(Mod)++".beam"))).