-module(crell_proc_links).

-export([
    run/1
]).

run(Pid) when is_pid(Pid) ->
    [{links, LinkedPids}] = erlang:process_info(Pid, [links]),
    run(LinkedPids, 1, #{}).

run(_, Count, _) when Count =< 0 ->
    [];
run(Pids, Count, AlreadySeen) when is_list(Pids) ->
    % [ run(Pid, Count, AlreadySeen) || Pid <- Pids ];
    Pids2SeenFOrmat = lists:filtermap(
        fun(Pid) ->
            case is_pid(maps:get(Pid, AlreadySeen, undefined)) of 
                true ->
                    false;
                false ->
                    {true, {Pid, []}}
            end
        end,
        Pids
    ),
    AlreadySeen2 = maps:merge(AlreadySeen, maps:from_list(Pids2SeenFOrmat)),
    Pids2 = element(1, lists:unzip(Pids2SeenFOrmat)),
    [ run(Pid, Count, AlreadySeen2) || Pid <- Pids2 ];
    % lists:filter_map(
    %     fun(Pid, ) ->
    %         case maps:get(Pid, AlreadySeen, undefined) of
    %             undefined ->

    %     end,
    %     AlreadySeen,
    %     Pids
    % )
run(Pid, Count, _) when is_pid(Pid) andalso Count =< 0 ->
    [];
run(Pid, _Count, _) when is_port(Pid) ->
    [];
run(Pid, Count, AlreadySeen) when is_pid(Pid) ->
    [{links, LinkedPids}] = erlang:process_info(Pid, [links]),
    #{ Pid => run(LinkedPids, Count-1, AlreadySeen)}.
    % lists:append(LinkedPids, run(LinkedPids, Count-1)).
