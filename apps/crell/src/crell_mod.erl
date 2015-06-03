-module(crell_mod).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-export([ handle_json/2
        ]).

-define(STATE,crell_mod_state).
-record(?STATE,{}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    ModList = crell_server:runtime_modules(),
    Json = to_json(ModList, []),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, Class, Reason}, _Req, _State) ->
    ok;
terminate(Reason, _Req, _State) ->
    ok.

to_json([],R) ->
    jsx:encode ( R );
to_json([{ModName,_ModPath}|T],R) ->
    to_json(T,[
        [
            {<<"module_name">>, list_to_binary(atom_to_list(ModName))}
            %% Some are preloaded... {erl_prim_loader,preloaded}
            %% {<<"module_path">>, list_to_binary(ModPath)}
        ] | R]
    ).
