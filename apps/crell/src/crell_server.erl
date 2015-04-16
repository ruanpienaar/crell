-module (crell_server).
-export([start_link/0,
         calc_app/1,
         calc_app/2,
         calc_proc/1,
         calc_proc/2,
         calc_app_env/1
        ]).
-export([remote_which_applications/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, crell_server_state).
-record(?STATE, { appmon_pid,
                  remote_node,
                  remote_node_cookie
                }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

calc_app(App) ->
    calc_app(App,[]).

calc_app(App,Opts) ->
    gen_server:call(?MODULE, {app,App,Opts}).

calc_proc(Pid) ->
    calc_proc(Pid,[]).

calc_proc(Pid,Opts) ->
    gen_server:call(?MODULE, {pid,Pid,Opts}).
   
calc_app_env(AppName) ->
   gen_server:call(?MODULE, {app_env,AppName}).
   
remote_which_applications() ->
   gen_server:call(?MODULE, remote_which_applications).

%% ---------------------------------------

init({}) ->
    {ok,Node} = application:get_env(crell, remote_node),
    {ok,Cookie} = application:get_env(crell, remote_cookie),
    case erlang:set_cookie(Node,Cookie) of 
    	true ->
    		true = net_kernel:connect(Node),
		    rpc:call(Node,crell_appmon,start_appmon,[]),
		    {ok, #?STATE{
		    	remote_node = Node,
		    	remote_node_cookie = Cookie
		    }};
		false ->
			{stop, {enoconnect,[{node,Node},{cookie,Cookie}]}}
	end.

handle_call({app,App,Opts}, _From, #?STATE{ remote_node = Node } = State) ->
    R = rpc:call(Node,crell_appmon,calc_app_tree,[App, Opts]),
    {reply, R, State};
handle_call({pid,Pid,Opts}, _From, #?STATE{ remote_node = Node } = State) ->
    R = rpc:call(Node,crell_appmon,calc_proc_tree,[Pid, Opts]),
    {reply, R, State};
handle_call({app_env,AppName}, _From, #?STATE{ remote_node = Node } = State) ->
    AppEnv = rpc:call(Node, application, get_all_env, [AppName]),
    {reply, {ok,AppEnv}, State};
handle_call(remote_which_applications, _From, #?STATE{ remote_node = Node } = State) ->
   Apps = rpc:call(Node, application, which_applications, []),
   {reply,Apps,State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call, ?MODULE}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
