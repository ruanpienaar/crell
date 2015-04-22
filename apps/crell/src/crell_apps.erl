-module(crell_apps).

-export([ init/3,
          rest_init/2,
          terminate/3 ]).

-define(STATE,crell_apps_state).
-record(?STATE,{}).

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

-export([init/3,
         content_types_provided/2,
         handle_json/2
        ]).

init(_Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #?STATE{ } }.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_json}], Req, State}.

handle_json(Req, State) ->
    % [{stdlib,"ERTS  CXC 138 10","2.3"},
    %  {kernel,"ERTS  CXC 138 10","3.1"}]
%     AppList = application:which_applications(),
    AppList = crell_server:remote_which_applications(),
    Json = to_json(AppList, []),
    {Json,Req,State}.

terminate(normal, _Req, _State) ->
    io:format("Terminate! ~p\n",[normal]),
    ok;
terminate({crash, Class, Reason}, _Req, _State) ->
    io:format("Terminate! ~p\n",[{crash, Class, Reason}]),
    ok;
terminate(Reason, _Req, _State) ->
    io:format("Terminate! ~p\n",[Reason]),
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
