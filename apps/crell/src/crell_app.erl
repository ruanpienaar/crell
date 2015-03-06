-module(crell_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case crell_sup:start_link() of
        {ok,S} ->
            {ok,_} = crell_web:start(),
            {ok,S};
        E ->
            E
    end.

stop(_State) ->
    ok.
