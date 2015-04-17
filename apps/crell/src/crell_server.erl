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
    {_, _} = est_rem_conn(Node, Cookie).
	
est_rem_conn(Node, COokie) ->
	case ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node, Cookie))) of
        true ->
		    start_remote_code(Node, Cookie);
        false ->
            {stop, {enoconnect,[{node,Node},{cookie,Cookie}]}}
    end.

start_remote_code(Node,Cookie) ->
	case application:get_env(crell, remote_action) of
		1 ->
			% try loading
			inject_module(crell_appmon,Node),
			rpc:call(Node, crell_appmon, start_appmon, []),
			{ok,state(Node, Cookie)};
		2 ->[B
			% try lib
			rpc:call(Node, crell_appmon, start_appmon, []),
			{ok,state(Node, Cookie)};
		3 ->
			% try loading, then lib,
			%% case inject_module(crell_appmon,Node) of 
			
			{ok,state(Node, Cookie)};
		undefined ->
			{ok,state(Node, Cookie)}
	end.

state(Node, Cookie) ->
	#?STATE{remote_node=Node, remote_node_cookie=Cookie}.

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

-spec inject_module(ModName :: term(), NodeName :: atom()) ->
                                        ok | {error, Reason :: term()}.
inject_module(ModName, NodeName) ->
    lager:info("Inject the agent code into the node (NodeName=~p, "
               "Agent=~p)", [NodeName, ModName]),
    case code:get_object_code(ModName) of
        {ModName, Bin, File} ->
            case rpc:call(NodeName, code, load_binary,
                          [ModName, File, Bin]) of
                {module, ModName} ->
                    purge_module(NodeName, ModName), % remove old code of module code
                    lager:info("Agent injected (NodeName=~p, Agent=~p)",
                               [NodeName, ModName]),
                    ok;
                {Error, Reason} when Error =:= error;
                                     Error =:= badrpc ->
                    lager:error("rpc(~p, code, load_binary, ...) "
                                "failed (ModName=~p, Reason=~p)",
                                [NodeName, ModName, Reason]),
                    {error, {load_binary_failed, Reason}}
            end;
        error ->
            lager:error("code:get_object_code failed (ModName=~p)",
                        [ModName]),
            {error, {get_object_code_failed, ModName}}
    end.