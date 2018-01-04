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
        [ [{<<"node">>, crell_web_utils:ens_bin(Node)},
           {<<"cookie">>, crell_web_utils:ens_bin(Cookie)},
           {<<"type">>, crell_web_utils:ens_bin(Type)}] || {Node,Cookie,Type} <- Nodes ]
    ),Req,State};
handle_json_path(Req, State, <<"/goanna_api/list_active_traces">>) ->
    ActiveTraces = goanna_api:list_active_traces(),
    {jsx:encode(
        [
         begin 
          {M,F,A,_MS} = 
              case TrcPattern of
                  {Mod} -> {Mod, undefined, undefined, undefined};
                  {Mod,Func} -> {Mod, Func, undefined, undefined};
                  {Mod,Func,Ari} -> {Mod, Func, Ari, undefined};
                  {Mod,Func,Ari,MatchS} -> {Mod, Func, Ari, MatchS}
              end,
          [{<<"node">>, crell_web_utils:ens_bin(ChildId)},
           %{<<"started">>, crell_web_utils:ens_bin(crell_web_utils:localtime_ms_str(Now))},
           {<<"module">>, crell_web_utils:ens_bin(M)},
           {<<"function">>, crell_web_utils:ens_bin(undef_function(F))},
           {<<"arity">>, crell_web_utils:ens_bin(undef_arity(A))},
           {<<"trace_opts">>, proplist_to_json_ready(Opts)}
          ]
        end
        || {{ChildId, TrcPattern}, Opts} <- ActiveTraces ]
    ),Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

undef_function(undefined) ->
    "*";
undef_function(F) ->
    F.

undef_arity(undefined) ->
    "*";
undef_arity(A) ->
    A.

proplist_to_json_ready(Props) ->
    lists:map(fun(KVPair) ->
        [KVPair]
    end, Props).
