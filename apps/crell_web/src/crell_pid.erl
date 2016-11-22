-module(crell_pid).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).

-export([handle_json/2
        ]).

-define(STATE,crell_pid_state).
-record(?STATE,{pid}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, _State) ->
    case cowboy_req:path(Req) of
        <<"/crell_proc/pids/">> ->
            {
                [{<<"application/json">>, handle_json}],
                Req,
                #?STATE{}
            };
        _Else ->
            URLPid = cowboy_req:binding(pid,Req), % <<"<50342.0.0>">>
            Pid = list_to_pid(binary_to_list(URLPid)),
            {
                [{<<"application/json">>, handle_json}],
                Req,
                #?STATE{pid = Pid}
            }
    end.

handle_json(Req, #?STATE{ pid = undefined} = State) ->
    {ok,Data} = crell_server:non_sys_processes(),
    Json = to_proc_proplist_json(Data),
    {Json,Req,State};
handle_json(Req, #?STATE{ pid = P } = State) when P /= undefined ->
    {ok,Data} = crell_server:calc_proc(State#?STATE.pid),
    Json = to_json(Data),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

to_proc_proplist_json(Procs) ->
    % Json =
    %     [{<<"processes">>, to_proc_proplist_json(Procs, [])}],
    jsx:encode(to_proc_proplist_json(Procs, [])).

to_proc_proplist_json([], Acc) ->
    Acc;
to_proc_proplist_json([[{pid,H},{name,Name},{mq,Qlen}]|T], Acc) ->
    ProcJsonStruct = [  {<<"pid">>,ensure_bin(H)},
                        {<<"name">>,ensure_bin(name_or_initcall(Name))},
                        {<<"mq">>,ensure_bin(Qlen)}
                    ],
    to_proc_proplist_json(T, [ ProcJsonStruct |Acc]).

name_or_initcall(Name) when is_atom(Name) ->
    Name;
name_or_initcall({M,F,Arity}) ->
    M.

% to_proc_proplist_json([]) ->
%     ok;
% to_proc_proplist_json([H|T]) ->
%     ok.

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
    V;
ensure_bin(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V)).
