-module(crell_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mnesia_init(),
    crell_sup:start_link().

stop(_State) ->
    ok.

%% Copy pasted from
%% https://github.com/ruanpienaar/pasture/blob/master/apps/pasture/src/pasture_db_mnesia.erl#L132
mnesia_init() ->
    mnesia:set_debug_level(verbose),
    io:format("Db init...\n"),
    %{ok,Master} = application:get_env(pasture, master_db_node),
    Master = node(),
    io:format("Master node : ~p\n",[Master]),
    MnesiaTbls=[crell_nodes],
    mnesia_init(MnesiaTbls,Master).

mnesia_init(MnesiaTbls,MasterNode) when MasterNode == node() ->
    Nodes = [node()],
    ExtraNodes = [],
    {ok,[]} = mnesia:change_config(extra_db_nodes, ExtraNodes),
    stopped = mnesia:stop(),
    io:format("Trying to install schema on ~p\n",[Nodes]),
    timer:sleep(25),
    case mnesia:create_schema(Nodes) of
        ok ->
            io:format("Schema created ...\n");
        {error,{NNN,{already_exists,NNN}}} ->
            io:format("Schema already created on ~p ...\n",[NNN])
    end,
    ok = mnesia:start(),
    io:format("Mnesia started...\n"),
    io:format("About to create tables : ~p\n", [MnesiaTbls]),
    lists:foreach(fun(Tbl) ->
        io:format("Creating table ~p ...\n",[Tbl]),
        case Tbl:create_table([MasterNode]) of
            {atomic,ok} ->
                io:format("Table ~p created ...\n", [Tbl]);
            Columns when is_list(Columns) ->
                io:format("Table ~p already created ...\n", [Tbl])
        end,
        lists:foreach(fun(EN) ->
            io:format("Add table copy ~p on node ~p ...\n",[Tbl, EN]),
            EnDiscCopies = rpc:call(EN, mnesia, table_info, [Tbl, disc_only_copies]),
            case lists:member(EN, EnDiscCopies) of
                true ->
                    io:format("Already has a copy\n");
                false ->
                    {atomic, ok} = mnesia:add_table_copy(Tbl, EN, disc_only_copies),
                    io:format("Added table copy of ~p to ~p ...\n", [Tbl, EN])
            end
        end, ExtraNodes)
    end, MnesiaTbls),
    ok = mnesia:wait_for_tables(MnesiaTbls, infinity).