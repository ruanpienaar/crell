-module(crell_proc).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-export([handle_json/2
        ]).

-define(STATE,crell_proc_state).
-record(?STATE,{app_name}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    AppName = cowboy_req:binding(app_name,Req),
    {[{<<"application/json">>, handle_json}], Req, #?STATE{ app_name = list_to_atom(binary_to_list(AppName)) }}.

handle_json(Req, State) ->
    {ok,Data} = crell_server:calc_app(State#?STATE.app_name),
    Json = to_json(Data),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

% Example Struct:
%          ---- two ---/--- four -- eight
% one --- /            \---- five
%         \              /--- six
%          ---- three ---\--- seven

% Example response:
% ex() ->
%     to_json({one,
%             [{five,five},
%              {six,six},
%              {seven,seven},
%              {eight,eight}],
%             [{one,two},
%              {one,three},
%              {two,four},
%              {two,five},
%              {three,six},
%              {three,seven},
%              {four,eight}],
%             []}).

to_json({Start,End,NotStartOrEnd,Remote}) ->
    jsx:encode(
        to_json_struct({Start,End,NotStartOrEnd,Remote})
    ).

to_json_struct({Start,End,NotStartOrEnd,Remote}) ->
    loop(Start,End,NotStartOrEnd,Remote).

loop(Pos,End,NotEnd,Remote) ->
    {{Pos,PosList},NotEnd2} = get_all(Pos,NotEnd,{Pos,[]}),
    [
     {<<"name">>,
      ensure_bin(Pos)
     },
     {<<"children">>,
      lists:reverse([loop(PL,End,NotEnd2,Remote) || PL <- PosList])
     }
    ].

get_all(K,L,{K,KList}) ->
    case lists:keyfind(K, 1, L) of
        {K,V} ->
            get_all(K,lists:keydelete(K,1,L),{K,[V|KList]});
        false ->
            {{K,KList},L}
    end.

ensure_bin(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V));
ensure_bin(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
ensure_bin(V) when is_list(V) ->
    list_to_binary(V);
ensure_bin(V) when is_binary(V) ->
    V.
