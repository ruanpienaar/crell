-module(crell_tcp_controller).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get_socket/0]).

-define(SERVER, ?MODULE).
-define(STATE, crell_tcp_controller_state).
-record(?STATE, { socket }).

start_link(Socket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Socket}, []).

init({Socket}) ->
    {ok, #?STATE{ socket = Socket }}.

handle_call(socket, _From, State) ->
    {reply, State#?STATE.socket, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------

get_socket() ->
    gen_server:call(?MODULE, socket).