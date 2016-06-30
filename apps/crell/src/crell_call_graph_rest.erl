-module (crell_call_graph_rest).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-export([ handle_json/2
        ]).

-define(STATE,crell_call_graph_rest_state).
-record(?STATE,{module}).

init(Req, Opts) ->
    Module = cowboy_req:binding(module,Req),
    {cowboy_rest, Req, #?STATE{ module = Module }}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    % io:format("~p \n", [State#?STATE.module]),
    Response = crell_call_graph:run(list_to_atom(binary_to_list(State#?STATE.module))),
    Json = to_json(Response),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, Class, Reason}, _Req, _State) ->
    ok;
terminate(Reason, _Req, _State) ->
    ok.

to_json(Response) ->
    jsx:encode( Response ).