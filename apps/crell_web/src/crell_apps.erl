-module(crell_apps).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-export([ handle_json/2
        ]).

-define(STATE,crell_apps_state).
-record(?STATE,{}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    {remote_running_applications, AppList}
        = crell_server:remote_which_applications(),
    Json = to_json(AppList, []),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

to_json([],R) ->
    jsx:encode ( R );
to_json([{AN,EV,AV}|T],R) ->
    to_json(T,[
        [
            {<<"application_name">>,    list_to_binary(atom_to_list(AN))},
            {<<"erts_version">>,        list_to_binary(EV)},
            {<<"application_version">>, list_to_binary(AV)}
        ] | R]
    ).
