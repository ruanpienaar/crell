-module(crell_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("crell_web.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % {ok,_} = crell_web:start()
    crell_web_sup:start_link().

stop(_State) ->
    ok.
