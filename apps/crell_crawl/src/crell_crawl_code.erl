-module (crell_crawl_code).

-export([start_link/0,
         recurse_dir/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([]).

-define(SERVER, ?MODULE).

%% ------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


%% Dir must END without a trailing '/'
recurse_dir(Dir) ->
    recurse_dir(Dir,Dir).

recurse_dir(PreFixDir,Dir) ->
    DirName = last_dir(Dir),
    case DirName of
        %% maybe make this configurable...
        Exclude when Exclude =:= ".git";
                     Exclude =:= ".svn" ->
            [];
        _ ->
            case file:list_dir(Dir) of
                {ok,Files} ->

                    %% abstract the json creation to a function,
                    %% and return a erlang TERM as the DIR Tree.

                    %% Use a Include(dirs/files), Exclude(dirs/files) Opts list,
                    %% ideally this should be a REGEXP

                    [{name,strip_prefix(PreFixDir,Dir)},
                     {children,
                        [begin
                            case filelib:is_dir(Dir++"/"++File) of
                                 false -> [{name,strip_prefix(PreFixDir,File)}];
                                 true  -> recurse_dir(PreFixDir, Dir++"/"++File)
                            end
                        end || File <- Files , File =/= DirName]
                    }];
                {error,enotdir} -> % trying to list a list_dir a file
                    io:format("\n Dir : ~p ERROR ~p \n",[Dir,enotdir]),
                    [];
                {error,Error} ->
                    io:format("\n Dir : ~p ERROR ~p \n",[Dir,Error]),
                    []
            end
    end.

strip_prefix(PreFixDir,PreFixDir) ->
    list_to_binary(last_dir(PreFixDir));
strip_prefix(PreFixDir,File) ->
    PreT = string:tokens(PreFixDir,"/"),
    T = string:tokens(File,"/"),
    F = fun(I) ->
        "/"++I
    end,
    list_to_binary(lists:map(F,T -- PreT)).

last_dir(Dir) ->
    lists:last(string:tokens(Dir,"/")).

%% ------------------

init({}) ->
    {ok, undefined}.

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