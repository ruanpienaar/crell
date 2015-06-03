-module (crell_server).
-export([start_link/0,
         calc_app/1,
         calc_app/2,
         calc_proc/1,
         calc_proc/2,
         calc_app_env/1,
         remote_which_applications/0,
         make_xref/0
]).
-export([
    runtime_modules/0,
    module_source/1
]).

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

runtime_modules() ->
    gen_server:call(?MODULE, runtime_modules).

%% ---------------------------------------

init({}) ->
    {ok,Node} = application:get_env(crell, remote_node),
    {ok,Cookie} = application:get_env(crell, remote_cookie),
    {_, _} = est_rem_conn(Node, Cookie).
	
est_rem_conn(Node, Cookie) ->
	case ((erlang:set_cookie(Node,Cookie)) andalso (net_kernel:connect(Node))) of
        true ->
		    start_remote_code(Node, Cookie);
        false ->
            {stop, {enoconnect,[{node,Node},{cookie,Cookie}]}}
    end.

start_remote_code(Node,Cookie) ->
	{ok,V} = application:get_env(crell, remote_action),
	case V of 
		1 ->
			% try loading module
			case inject_module(crell_appmon, Node) of 
				ok ->			
					%% TODO: know that it's started properly
					rpc:call(Node, crell_appmon, start_appmon, []),
					{ok,state(Node, Cookie)};
				_ ->
					{stop, enotloaded}
			end;
		2 ->
			% TODO: try lib
			%% rpc:call(Node, crell_appmon, start_appmon, []),
			{ok,state(Node, Cookie)};
		3 ->
			% TODO: try loading, then lib,
			%% case inject_module(crell_appmon,Node) of 
			%%% TODO: complete it
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
   {reply, Apps, State};
handle_call(runtime_modules, _From, #?STATE{ remote_node = Node } = State) ->
    Modules = rpc:call(Node, code, all_loaded, []),
    {reply, Modules, State};
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
    
  purge_module(Node, Module) ->
    Res = try rpc:call(Node, code, soft_purge, [Module]) of
              true ->
                  ok;
              false ->
                  hard_purge_module(Node, Module);
              {badrpc, _} = RPCError ->
                  {error, RPCError}
          catch
              C:E ->
                  {error, {C,E}}
          end,
    case Res of
        ok ->
            lager:info("Purged ~p from ~p", [Module, Node]);
        {error, Error} ->
            lager:error("Error while purging  ~p from ~p: ~p", [Module, Node, Error])
    end,
    ok.

hard_purge_module(Node, Module) ->
    try rpc:call(Node, code, purge, [Module]) of
        true ->
            lager:info("Purging killed processes on ~p while loading ~p", [Node, Module]),
            ok;
        false ->
            ok;
        {badrpc, _} = RPCError ->
            {error, RPCError}
    catch
        C:E ->
            {error, {C,E}}
    end.

%% This is to follow a function to another....
make_xref() ->
    ok.

module_functions(Mod) ->
    Exports = Mod:module_info(exports),
    Unexported = [F || F <- Mod:module_info(functions), not lists:member(F, Exports)],
    {Mod, Exports, Unexported}.

module_source(Module) ->
  get_source(Module).

-spec abstract_code(module()) -> [erl_parse:abstract_form()].
abstract_code(Module) ->
        File = code:which(Module),
        {ok,{_Mod,[{abstract_code,{_Version,Forms}}]}} = beam_lib:chunks(File, [abstract_code]),
        Forms.
 
mod_src(Module) ->
    Forms = abstract_code(Module),
    lists:flatten([[erl_pp:form(F),$\n] || F <- Forms, element(1,F) =:= attribute orelse element(1,F) =:= function]).
 
fun_src(Mod, Fun, Arity) ->
    Forms = abstract_code(Mod),
    [FF] = [FF || FF = {function, _Line, Fun2, Arity2, _} <- Forms, Fun2 =:= Fun, Arity2 =:= Arity],
    lists:flatten(erl_pp:form(FF)).
 
-spec get_source(module() | {module(), function(), Arity :: non_neg_integer()}) -> {ok, string()} | {error, Reason :: any()}.
get_source(What) ->
        try
        case What of
                {M,F,A} ->
                        {ok, fun_src(M,F,A)};
                Mod when is_atom(Mod)->
                        {ok, mod_src(Mod)}
        end
        catch _:E ->
                {error, E}
        end.
