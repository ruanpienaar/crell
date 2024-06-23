-module(crell_web_utils).
-export([
    ens_bin/1, ens_atom/1, ens_int/1,
    localtime_ms_str/1,
    map_from_proplist_r/1,
    jsx_safe_string/1
]).

ens_bin(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V));
ens_bin(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
ens_bin(V) when is_list(V) ->
    try
        list_to_binary(V)
    catch _:_:_ ->
        ens_bin(io_lib:format("~p", [V]))
    end;
ens_bin(V) when is_binary(V) ->
    V;
ens_bin(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
ens_bin(V) when is_tuple(V) ->
    ens_bin(io_lib:format("~p", [V])).

ens_atom(V) when is_list(V) ->
    list_to_atom(V);
ens_atom(V) when is_binary(V) ->
    list_to_atom(binary_to_list(V));
ens_atom(V) when is_atom(V) ->
    V.

ens_int(V) when is_list(V) ->
    list_to_integer(V);
ens_int(V) when is_binary(V) ->
    binary_to_integer(V);
ens_int(V) when is_integer(V) ->
    V.

localtime_ms_str(Now) ->
    {{Yy,Mm,Dd}, {Hours, Minutes, Seconds, MicroS}} = localtime_ms(Now),
    io_lib:format("~p-~p-~p ~p:~p:~p.~p", [Yy,Mm,Dd,Hours,Minutes,Seconds,MicroS]).

localtime_ms(Now) ->
    {_, _, Micro} = Now,
    {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    {Date, {Hours, Minutes, Seconds, Micro div 1000 rem 1000}}.


map_from_proplist_r(Proplist) when is_list(Proplist) ->
    Map = proplists:to_map(Proplist),
    case length(maps:keys(Map)) of
        0 ->
            map_from_proplist_r(Map);
        _ ->
            maps:fold(
                fun
                (K, V, A) when is_tuple(V) orelse is_pid(V) orelse is_reference(V) ->
                    A#{ K => jsx_safe_string(V) };
                (K, V, A) when is_list(V) ->
                    case proplists:get_keys(V) of
                        [] ->
                            A#{ jsx_safe_string(K) => jsx_safe_string(V) };
                        _Keys ->
                            A#{ jsx_safe_string(K) => map_from_proplist_r(V) }
                    end;
                (K, V, A) ->
                    A#{ K => V }
                end,
                #{},
                Map
            )
    end;
map_from_proplist_r(Term) ->
    jsx_safe_string(Term).

% Brother uhhhhhh :(
jsx_safe_string(Term) ->
    binary:list_to_bin(io_lib:format("~p", [Term])).

%    [{current_function,{dist_util,con_loop,2}},
%     {initial_call,{inet_tcp_dist,do_accept,7}},
%     {status,waiting},
%     {message_queue_len,0},
%     {links,[#Port<34786.4506>,<34786.64.0>]},
%     {dictionary,[]},
%     {trap_exit,false},
%     {error_handler,error_handler},
%     {priority,max},
%     {group_leader,<34786.46.0>},
%     {total_heap_size,1597},
%     {heap_size,987},
%     {stack_size,8},
%     {reductions,1915},
%     {garbage_collection,
%         [{max_heap_size,
%              #{error_logger => true,
%                include_shared_binaries => false,
%                kill => true,size => 0}},
%          {min_bin_vheap_size,46422},
%          {min_heap_size,233},
%          {fullsweep_after,65535},
%          {minor_gcs,3}]},
%     {suspending,[]}]

% #{current_function => {dist_util,con_loop,2},
%     dictionary => [],error_handler => error_handler,
%     garbage_collection =>
%         [{max_heap_size,#{error_logger => true,include_shared_binaries => false,
%                           kill => true,size => 0}},
%          {min_bin_vheap_size,46422},
%          {min_heap_size,233},
%          {fullsweep_after,65535},
%          {minor_gcs,3}],
%     group_leader => <34786.46.0>,heap_size => 987,
%     initial_call => {inet_tcp_dist,do_accept,7},
%     links => [#Port<34786.4506>,<34786.64.0>],
%     message_queue_len => 0,priority => max,reductions => 1915,
%     stack_size => 8,status => waiting,suspending => [],
%     total_heap_size => 1597,trap_exit => false}