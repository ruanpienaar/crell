-module(crell_app_env).

-export([ init/2,
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

init(_Req, []) ->
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

to_json(Data) ->
    jsx:encode(Data).
    
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
