-module(crell_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    crell_web = ets:new(crell_web, [ordered_set, named_table]),
    case crell_sup:start_link() of
        {ok,S} ->
            {ok, CowboyListener} = crell_web:start(),
            true = ets:insert(crell_web, {CowboyListener}),
            {ok,S};
        E ->
            E
    end.

stop(_State) ->
    CowboyListener = ets:first(crell_web),
    crell_web:stop(CowboyListener),
    true = ets:delete(crell_web),
    ok.
