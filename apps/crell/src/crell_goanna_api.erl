-module(crell_goanna_api).
-export([ init/2,
          terminate/3,
          content_types_provided/2, %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
          resource_exists/2
]).

-export([ handle_json/2
        ]).

% -define(STATE,crell_traces_state).
% -record(?STATE,{path}).

-include_lib("goanna/include/goanna.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

resource_exists(Req,State) ->
    Path = cowboy_req:path(Req),
    {url_exists(Path),Req,State}.

url_exists(<<"/goanna_api/nodes">>) ->
    true;
url_exists(<<"/goanna_api/list_active_traces">>) ->
    true;
url_exists(Path) ->
    io:format("[~p] Unknown path: ~p", [?MODULE, Path]),
    false.

handle_json(Req, State) ->
    handle_json_path(Req, State, cowboy_req:path(Req)).

%% TODO: i maybe don't have to always make it binary, since it's JSX now...
handle_json_path(Req, State, <<"/goanna_api/nodes">>) ->
    Nodes = goanna_api:nodes(),
    {jsx:encode(
        [ [{<<"node">>, ensure_bin(Node)},
           {<<"cookie">>, ensure_bin(Cookie)},
           {<<"type">>, ensure_bin(Type)}] || {Node,Cookie,Type} <- Nodes ]
    ),Req,State};
handle_json_path(Req, State, <<"/goanna_api/list_active_traces">>) ->
    ActiveTraces = goanna_api:list_active_traces(),
    {jsx:encode(
        [ [{<<"node">>, ensure_bin(Node)},
           {<<"started">>, ensure_bin(localtime_ms_str(Now))},
           {<<"module">>, ensure_bin(TrcPattern#trc_pattern.m)},
           {<<"function">>, ensure_bin(undef_function(TrcPattern#trc_pattern.f))},
           {<<"arity">>, ensure_bin(undef_arity(TrcPattern#trc_pattern.a))},
           {<<"trace_opts">>, proplist_to_json_ready(TrcOpts)}
          ] || {{Node,TrcPattern},Now,TrcOpts} <- ActiveTraces ]
    ),Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

ensure_bin(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V));
ensure_bin(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
ensure_bin(V) when is_list(V) ->
    list_to_binary(V);
ensure_bin(V) when is_binary(V) ->
    V;
ensure_bin(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V)).

undef_function(undefined) ->
    "*";
undef_function(F) ->
    F.

undef_arity(undefined) ->
    "/*";
undef_arity(A) ->
    A.

localtime_ms_str(Now) ->
    {{Yy,Mm,Dd}, {Hours, Minutes, Seconds, MicroS}} = localtime_ms(Now),
    io_lib:format("~p-~p-~p ~p:~p:~p.~p", [Yy,Mm,Dd,Hours,Minutes,Seconds,MicroS]).

localtime_ms(Now) ->
    {_, _, Micro} = Now,
    {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    {Date, {Hours, Minutes, Seconds, Micro div 1000 rem 1000}}.

proplist_to_json_ready(Props) ->
    lists:map(fun(KVPair) ->
        [KVPair]
    end, Props).
