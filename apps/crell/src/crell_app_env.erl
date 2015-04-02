-module(crell_app_env).

-export([ init/3,
          rest_init/2,
          terminate/3 ]).

-define(STATE,crell_app_env_state).
-record(?STATE,{app_name}).

%% All available REST handler exports:
 -export([
%     allowed_methods/2        %% Methods : all
%     allow_missing_post/2     %% Methods : POST
%     charsets_provided/2      %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     content_types_accepted/2 %% Methods : POST, PUT, PATCH
    content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     delete_completed/2       %% Methods : DELETE
%     delete_resource/2        %% Methods : DELETE
%     expires/2                %% Methods : GET, HEAD
%     forbidden/2              %% Methods : all
%     generate_etag/2          %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     is_authorized/2          %% Methods : all
%     is_conflict/2            %% Methods : PUT
%     known_methods/2          %% Methods : all
%     languages_provided/2     %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     last_modified/2          %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     malformed_request/2      %% Methods : all
%     moved_permanently/2      %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     moved_temporarily/2      %% Methods : GET, HEAD, POST, PATCH, DELETE
%     multiple_choices/2       %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     options/2                %% Methods : OPTIONS
%     previously_existed/2     %% Methods : GET, HEAD, POST, PATCH, DELETE
%     resource_exists/2        %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     service_available/2      %% Methods : all
%     uri_too_long/2           %% Methods : all
%     valid_content_headers/2  %% Methods : all
%     valid_entity_length/2    %% Methods : all
%     variances/2              %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
 ]).

-export([handle_json/2
        ]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {AppName, Req1} = cowboy_req:binding(app_name,Req),
    {ok, Req1, #?STATE{ app_name = list_to_atom(binary_to_list(AppName)) } }.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    {ok,Data} = crell_server:calc_app_env(State#?STATE.app_name),
    Json = to_json(Data),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    % io:format("Terminate! ~p\n",[normal]),
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    % io:format("Terminate! ~p\n",[{crash, Class, Reason}]),
    ok;
terminate(_Reason, _Req, _State) ->
    % io:format("Terminate! ~p\n",[Reason]),
    ok.

%          ---- two ---/--- four -- eight
% one --- /            \---- five
%         \              /--- six
%          ---- three ---\--- seven

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

%% App mon info:

% {ok,{"<0.1199.0>",
%      [{<0.1223.0>,"<0.1223.0>"},
%       {<0.1224.0>,"kernel_safe_sup"},
%       {<0.1204.0>,"<0.1204.0>"},
%       {<0.1200.0>,"<0.1200.0>"},
%       {<0.1211.0>,"<0.1211.0>"},
%       {<0.1312.0>,"inet_gethost_native_sup"},
%       {<0.1203.0>,"global_name_server"},
%       {<0.1199.0>,"<0.1199.0>"},
%       {<0.1210.0>,"net_kernel"},
%       {<0.1220.0>,"user"},
%       {<0.1313.0>,"inet_gethost_native"},
%       {<0.1252.0>,"timer_server"},
%       {<0.1212.0>,"<0.1212.0>"},
%       {<0.1205.0>,"<0.1205.0>"},
%       {<0.1221.0>,"<0.1221.0>"},
%       {<0.1213.0>,"global_group"},
%       {<0.1215.0>,"code_server"},
%       {<0.1217.0>,"standard_error"},
%       {<0.1201.0>,"kernel_sup"},
%       {<0.1218.0>,"<0.1218.0>"},
%       {<0.1206.0>,"inet_db"},
%       {<0.1207.0>,"net_sup"},
%       {<0.1209.0>,"auth"},
%       {<0.1214.0>,"file_server_2"},
%       {<0.1219.0>,"user_drv"},
%       {<0.1202.0>,"rex"},
%       {<0.1208.0>,"erl_epmd"},
%       {<0.1216.0>,"standard_error_sup"}],
%      [{"kernel_safe_sup","timer_server"},
%       {"kernel_safe_sup","inet_gethost_native_sup"},
%       {"<0.1200.0>","kernel_sup"},
%       {"inet_gethost_native_sup","inet_gethost_native"},
%       {"global_name_server","<0.1204.0>"},
%       {"global_name_server","<0.1205.0>"},
%       {"<0.1199.0>","<0.1200.0>"}, <------------------------------------
%       {"net_kernel","<0.1211.0>"},
%       {"net_kernel","<0.1212.0>"},
%       {"net_kernel","port 19608"},
%       {"user","user_drv"},
%       {"inet_gethost_native","port 39672"},
%       {"standard_error","port 39520"},
%       {"kernel_sup","code_server"},
%       {"kernel_sup","<0.1218.0>"},
%       {"kernel_sup","<0.1223.0>"},
%       {"kernel_sup","kernel_safe_sup"},
%       {"kernel_sup","standard_error_sup"},
%       {"kernel_sup","inet_db"},
%       {"kernel_sup","global_group"},
%       {"kernel_sup","file_server_2"},
%       {"kernel_sup","net_sup"},
%       {"kernel_sup","rex"},
%       {"kernel_sup","global_name_server"},
%       {"<0.1218.0>","user"},
%       {"net_sup","erl_epmd"},
%       {"net_sup","auth"},
%       {"net_sup","net_kernel"},
%       {"file_server_2","port 19632"},
%       {"user_drv","<0.1221.0>"},
%       {"user_drv","port 39528"},
%       {"erl_epmd","port 19624"},
%       {"standard_error_sup","standard_error"}],
%      [{"timer_server","cowboy_clock"},
%       {"timer_server","<0.1314.0>"}]}}













%----------------------

% allowed_methods(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% allow_missing_post(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% charsets_provided(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% content_types_accepted(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% content_types_provided(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% delete_completed(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% delete_resource(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% expires(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% forbidden(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% generate_etag(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% is_authorized(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% is_conflict(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% known_methods(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% languages_provided(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% last_modified(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% malformed_request(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% moved_permanently(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% moved_temporarily(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% multiple_choices(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% options(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% previously_existed(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% resource_exists(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% service_available(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% uri_too_long(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% valid_content_headers(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% valid_entity_length(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% variances(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
