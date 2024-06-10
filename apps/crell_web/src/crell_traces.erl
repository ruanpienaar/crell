-module(crell_traces).

% -export([ init/2,
%           terminate/3,
%           content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
% ]).

% -export([ handle_json/2
%         ]).

% -define(STATE,crell_traces_state).
% -record(?STATE,{}).

% init(Req, Opts) ->
%     {cowboy_rest, Req, Opts}.

% content_types_provided(Req, State) ->
%     {[{<<"application/json">>, handle_json}], Req, State}.

% handle_json(Req, State) ->
%     Json =
%         case crell_server:get_eb_traces() of
%             {ok,{undefined,[]}} ->
%                 to_json([]);
%             {ok,{undefined,Traces}} ->
%                 to_json(Traces);
%             {ok, {_RedbugPid, Traces}} ->
%                 to_json(Traces)
%         end,
%     {Json,Req,State}.

% terminate(normal, _Req, _State) ->
%     ok;
% terminate({crash, Class, Reason}, _Req, _State) ->
%     ok;
% terminate(Reason, _Req, _State) ->
%     ok.

% %% TODO: call, crell_server, get_traces,
% % and show them on the page...
% % maybe use websocket???

% to_json(Traces) ->
%     to_json(Traces, []).

% to_json([], R) ->
%     jsx:encode(lists:reverse(R));
% to_json([H|T], R) ->

%     % S = io_lib:format("~s\n", [io_lib:format("~p", [H])]),

% %% all the diff cases of prfTrc.erl........
%     S =
%         case H of

%             % send

%             % recv


%             % {retn,{{crell_remote,test_function2,2},ok},
%             %           {<49626.38.0>,{erlang,apply,2}},
%             %           {12,35,45,636897}}
%             {MsgType='retn',{{M,F,A},V},{P,InitCall},TS} ->
%                 MfaBin = mfa_to_bin({M,F,A}),
%                 [{<<"msg">>, list_to_binary(atom_to_list(MsgType))},
%                  {<<"mfa">>, MfaBin},
%                  {<<"p">>, list_to_binary(pid_to_list(P))},
%                  {<<"p_ic">>, init_call_to_bin(InitCall)},
%                  {<<"timestamp">>, ts_to_bin(TS)}
%                 ];
%             {MsgType='call',{{M,F,A},<<>>},{P,InitCall},TS} ->
%                 MfaBin = mfa_to_bin({M,F,A}),
%                 [{<<"msg">>, list_to_binary(atom_to_list(MsgType))},
%                  {<<"mfa">>, MfaBin},
%                  {<<"b">>, <<>>},
%                  {<<"p">>, list_to_binary(pid_to_list(P))},
%                  {<<"p_ic">>, init_call_to_bin(InitCall)},
%                  {<<"timestamp">>, ts_to_bin(TS)}
%                 ];
%             {MsgType='call',{{M,F,A},B},{P,InitCall},TS} ->
%                 MfaBin = mfa_to_bin({M,F,A}),
%                 [{<<"msg">>, list_to_binary(atom_to_list(MsgType))},
%                  {<<"mfa">>, MfaBin},
%                  {<<"b">>, sanitize(B)},
%                  {<<"p">>, list_to_binary(pid_to_list(P))},
%                  {<<"timestamp">>, ts_to_bin(TS)}
%                 ]
%         end,
%     to_json(T,[S|R]).

% %% TODO: probably use io_lib:format for all the below string operations...
% mfa_to_bin({M,F,A}) ->
%     list_to_binary(
%         io_lib:format("~p:~p~p", [M, F, A])
%     ).
%     % list_to_binary(atom_to_list(M) ++ ":" ++
%     %                atom_to_list(F) ++
%     %                args_to_str(A)).

% args_to_str(A) when is_integer(A) ->
%     "/"++integer_to_list(A);
% args_to_str(A) when is_list(A) ->
%     %% TODO: might have to remove the last comma
%     lists:foldl(fun(I,Acc) -> Acc ++ ensure_list(I) ++ "," end, "(", A) ++ ")".

% ts_to_bin({H,M,S,MS}) ->
%     list_to_binary(
%         integer_to_list(H)++":"++
%         integer_to_list(M)++":"++
%         integer_to_list(S)++"."++
%         integer_to_list(MS)).

% init_call_to_bin({M,F,A}) ->
%     mfa_to_bin({M,F,A});
% init_call_to_bin(dead) ->
%     <<"dead">>.

% ensure_list(A) when is_atom(A) ->
%     atom_to_list(A);
% ensure_list(A) when is_integer(A) ->
%     integer_to_list(A);
% ensure_list(A) when is_binary(A) ->
%     binary_to_list(A);
% ensure_list(A) when is_list(A) ->
%     A.

% % sanitize(B) ->
% %     B.
% sanitize(B) ->
%     BStr = binary_to_list(B),
%     BStr1 = re:replace(BStr, "\\\\n", "\n", [global, {return, list}]),
%     list_to_binary(BStr1).
