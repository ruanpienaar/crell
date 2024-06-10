%% TODO: Why is this module not called crell_mods?
%% Find some standard way of splitting the REST cowyboy mods

%% TODO: Cache this JSON response, and use that properly......
%% so that the page loads are quicker

%% TODO: make an option to exclude OTP related modules...

-module(crell_mod).

% -export([ init/2,
%           terminate/3,
%           content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
% ]).

% -export([ handle_json/2
%         ]).

% -define(STATE,crell_mod_state).
% -record(?STATE,{}).

% init(Req, Opts) ->
%     {cowboy_rest, Req, Opts}.

% content_types_provided(Req, State) ->
%     {[{<<"application/json">>, handle_json}], Req, State}.

% handle_json(Req, State) ->
%     {remote_modules,MFs} = crell_server:runtime_modules(),
%     Json = to_json(MFs, []),
%     {Json,Req,State}.

% terminate(normal, _Req, _State) ->
%     ok;
% terminate({crash, Class, Reason}, _Req, _State) ->
%     ok;
% terminate(Reason, _Req, _State) ->
%     ok.

% to_json([],R) ->
%     jsx:encode ( R );
% to_json([{ModName,Exports}|T],R) ->
%     to_json(T,[
%         [
%             {<<"m">>,list_to_binary(atom_to_list(ModName))},
%             {<<"fs">>,
%                 [
%                  [{<<"f">>, list_to_binary(atom_to_list(FunctionName))},
%                   {<<"a">>, Arity}]
%                 || {FunctionName, Arity} <- Exports ]
%             }
%             %% Some are preloaded... {erl_prim_loader,preloaded}
%             %% {<<"module_path">>, list_to_binary(ModPath)}
%         ] | R]
%     ).
