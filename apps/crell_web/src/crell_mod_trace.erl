-module(crell_mod_trace).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-export([ handle_json/2
        ]).

-define(STATE,crell_mod_trace_state).
-record(?STATE,{module}).

init(Req, Opts) ->
    Module = cowboy_req:binding(module,Req),
    {cowboy_rest, Req, #?STATE{ module = Module }}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
	M = sanitise_input({module, State#?STATE.module}),
    {ok, Pid} = crell_server:redbug_trace(M),
    Json = jsx:encode([{response, true}]),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, Class, Reason}, _Req, _State) ->
    ok;
terminate(Reason, _Req, _State) ->
    ok.

to_json(Response) ->
    jsx:encode( Response ).
    
sanitise_input({module, M}) ->
	list_to_atom(binary_to_list(M)).