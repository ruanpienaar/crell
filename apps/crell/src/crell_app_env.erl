-module(crell_app_env).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).
-export([ handle_json/2
]).

-define(STATE,crell_app_env_state).
-record(?STATE,{app_name}).

init(Req, Opts) ->
	AppName = cowboy_req:binding(app_name,Req),
    {cowboy_rest, Req, #?STATE{ app_name = list_to_atom(binary_to_list(AppName)) }}.

content_types_provided(Req, State) ->
	
	{[{<<"application/json">>, handle_json}], Req, State}.

    %% {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    {ok,Data} = crell_server:calc_app_env(State#?STATE.app_name),
    Json = to_json(Data),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

to_json(Data) ->
    jsx:encode(Data).
