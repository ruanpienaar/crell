-module(crell_crawl_dir).

-export([ init/2,
          terminate/3,
          content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
]).  
-export([handle_json/2
]).

-define(STATE,crell_crawl_dir_state).
-record(?STATE,{dir}).

init(Req, Opts) ->
	DirName = cowboy_req:binding(dir_name, Req),
    {cowboy_rest, Req, #?STATE{ dir = binary_to_list(DirName) }}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>,handle_json}], Req, State}.

handle_json(Req, State) ->
    Method = cowboy_req:method(Req),
    do_handle_json(Req,State,Method).

do_handle_json(Req,State,<<"GET">>) ->
    D = State#?STATE.dir,
    DirStruct = crell_crawl_code:recurse_dir(D),
    Json = jsx:encode(DirStruct),
    {Json,Req,State};
do_handle_json(Req,State,<<"POST">>) ->
    SetBodyReq = cowboy_req:set_resp_body(<<"SomeBody!">>, Req),
    { {true,"path/to/resource"}, SetBodyReq, State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.
