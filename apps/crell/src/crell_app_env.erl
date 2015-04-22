-module(crell_app_env).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-define(STATE,crell_app_env_state).
-record(?STATE,{app_name}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	io:format("State : ~p\n",[State]),
    AppName = cowboy_req:binding(app_name,Req),
    {[{<<"application/json">>, handle_json}], Req, #?STATE{ app_name = list_to_atom(binary_to_list(AppName)) }}.

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
