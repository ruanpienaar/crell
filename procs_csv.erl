-module(procs_csv).
-export([do/2, links/2]).

-define(SEP, "§").

%% ---------

links(N, Cookie) ->
    trycreate([procs]),

    true = erlang:set_cookie(N, Cookie),
    true = net_kernel:connect(N),
    Procs = rpc:call(N, erlang, processes, []),
    Infos=[links, registered_name],

    {ok, FPID} = file:open("process_links.csv", [write]),

    % fetch the remote data
    
    lists:foreach(fun(Pid) ->
        case rpc:call(N, erlang, process_info, [Pid, Infos]) of
            undefined ->
                [];
            [{links,Links},{registered_name,RegName}] ->
                Regname2 = 
                    case RegName of
                        [] -> Pid;
                        R  -> R
                    end,
                true = ets:insert(procs, {Pid, Regname2, Links}) 
        end
    end, Procs),

    % create heading
    ok = file:write(FPID, 
        create_line([{source, "source"},
                     {target, "target"},
                     {value, "value"}])),

    % create the data lines
    ok = 
        lists:foreach(fun
         ([]) ->
            ok;
         ({_Pid, RegName, Links}) ->
            lists:foreach(fun(Link) when is_port(Link) ->
                ok;
                             (Link)  ->
                case ets:lookup(procs, Link) of
                    [{Link, TargetRegName, _ }] ->
                        %% Links are bi-directional, so you could potentially
                        %% delete a entry once' you've looked it up.
                        L = create_line([{source,ensure_list(RegName)},
                                         {target,ensure_list(TargetRegName)},
                                         {value,"1"}]),
                        ok = file:write(FPID, L);
                    [] ->
                        % io:format("Cannot find :~p\n", [Link])
                end
            end, Links)
        end, ets:tab2list(procs)),
    file:close(FPID).

%% ---------

do(N, Cookie) ->
    true = erlang:set_cookie(N, Cookie),
    true = net_kernel:connect(N),
    Procs = rpc:call(N, erlang, processes, []),
    Infos=[
    	   %%Bbacktrace,
           % binary,
           % catchlevel,
           % current_function,
           % current_location,
           % current_stacktrace,
           % dictionary,
           % error_handler,
           % garbage_collection,
           % group_leader,
           % heap_size,
           % initial_call,
           % links,
           % last_calls,
           memory
           %%,
           % message_queue_len,
           % messages,
           % min_heap_size,
           % min_bin_vheap_size,
           % monitored_by,
           % monitors,
           % priority,
           % reductions,
           % registered_name,
           % sequential_trace_token,
           % stack_size,
           % status,
           % suspending,
           % total_heap_size,
           % trace,
           % trap_exit
           ],

    {ok, FPID} = file:open("process_info_results.csv", [write]),

    % fetch the remote data
    ProcsAndInfo = 
        lists:map(fun(Pid) ->
            case rpc:call(N, erlang, process_info, [Pid, Infos]) of
                undefined ->
                    [];
                ProcInfos ->
                    {Pid,ProcInfos}
            end
        end, Procs),

    % create heading
    file:write(FPID, 
        create_line( [{pid,pid}] ++ lists:zip(Infos, Infos) )),

    % create the data lines
    ok = 
        lists:foreach(fun
         ([]) ->
            ok;
         ({Pid,ProcInfos}) -> 
            ok = file:write(FPID, 
                create_line([{pid,Pid}] ++ ProcInfos))
        end, ProcsAndInfo),
    file:close(FPID).

create_line(Is) ->
    create_line(Is,[]).

create_line([], R) ->
    lists:reverse(R) ++ "\n";
create_line([{_Info, InfoVal}|T=[]], R) ->
    create_line(T, [ensure_list(InfoVal)|R]);
% create_line([[]|T], R) ->
%     create_line(T,R);
create_line([{_Info, InfoVal}|T], R) ->
    create_line(T, [ensure_list(InfoVal) ++ ?SEP|R]).
    
ensure_list(A) when is_atom(A) ->
    atom_to_list(A);
ensure_list(A) when is_integer(A) ->
    integer_to_list(A);
ensure_list(A) when is_tuple(A) ->
    tuple_to_list(A);
ensure_list(A) when is_pid(A) ->
    pid_to_list(A);
ensure_list(A) when is_binary(A) ->
    binary_to_list(A);
ensure_list(A) when is_port(A) ->
    erlang:port_to_list(A);
ensure_list(A) when is_reference(A) ->
    erlang:ref_to_list(A);
ensure_list(A) when is_map(A) ->
    maps:to_list(A);
ensure_list(A) when is_list(A) ->
    A.

trycreate(Tbls) ->  
    lists:foreach(fun(TblName) -> 
        case ets:info(TblName) of 
            undefined -> ets:new(TblName, [named_table, ordered_set]);
            _Props    -> ok
        end
    end, Tbls).
