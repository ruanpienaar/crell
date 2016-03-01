-module(crell_traces).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-export([ handle_json/2
        ]).

-define(STATE,crell_traces_state).
-record(?STATE,{}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    Json =
        case crell_server:get_traces() of
            {ok,{undefined,[]}} ->
                ok;
            {ok,{undefined,Traces}} ->
                to_json(Traces);
            {ok, {_RedbugPid, Traces}} ->
                to_json(Traces)
        end,
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, Class, Reason}, _Req, _State) ->
    ok;
terminate(Reason, _Req, _State) ->
    ok.

%% TODO: call, crell_server, get_traces,
% and show them on the page...
% maybe use websocket???

to_json(Traces) ->
    to_json(Traces, []).

to_json([], R) ->
    jsx:encode(R);
to_json([H|T], R) ->
    to_json(T,[
        [
            {<<"trcline">>, list_to_binary(io_lib:format("~p", [H]))}
        ]
        | R]).

% to_json([],R) ->
%     jsx:encode ( R );
% to_json([{ModName,Exports}|T],R) ->
%     to_json(T,[
%         [
%             {<<"m">>,list_to_binary(atom_to_list(ModName))},
%             {<<"fs">>,
%                 [
%                  [{<<"f">>, list_to_binary(atom_to_list(FunctionName))},
%                   {<<"a">>, Arity}]
%                 || {FunctionName, Arity} <- Exports ]
%             }
%             %% Some are preloaded... {erl_prim_loader,preloaded}
%             %% {<<"module_path">>, list_to_binary(ModPath)}
%         ] | R]
%     ).
