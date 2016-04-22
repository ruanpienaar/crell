% /home/ci/erlang/17.4/bin/erl -sname debug@localhost -setcookie DJGVFBUEDATQLOHXTLXD -remsh liability_cc_eunit@localhost

-module(process_info_report).
-export([do/2]).

do(N, Cookie) ->
    try
        backtraces=ets:new(backtraces, [named_table])
    catch
        _C:_E ->
            true = ets:delete(backtraces),
            backtraces=ets:new(backtraces, [named_table])
    end,
    true = erlang:set_cookie(N, Cookie),
    true = net_kernel:connect(N),
    Procs = rpc:call(N, erlang, processes, []),
    Infos=[backtrace,
           binary,
           catchlevel,
           current_function,
           current_location,
           current_stacktrace,
           dictionary,
           error_handler,
           garbage_collection,
           group_leader,
           heap_size,
           initial_call,
           links,
           last_calls,
           memory,
           message_queue_len,
           messages,
           min_heap_size,
           min_bin_vheap_size,
           monitored_by,
           monitors,
           priority,
           reductions,
           registered_name,
           sequential_trace_token,
           stack_size,
           status,
           suspending,
           total_heap_size,
           trace,
           trap_exit],
    % Infos=[current_function, trap_exit],

    {ok, FPID} = file:open("process_info_results.html", [write]),
    ok = file:write(FPID, create_part1()),

    %% TODO: first create a small table of all pids, "which are clickable links, with anchors to the table below:"
    ok = file:write(FPID, "<table class=\"table-hover table-bordered\" style=\"table-layout: fixed; width: 100%\" ><captian>Process list</captian><thead></thead><tbody><tr><td>|"),
    lists:foreach(fun(Proc) -> 
        ok = file:write(FPID, "<a href=\"#" ++ ensure_list(Proc) ++ "\">" ++ pid_to_list(Proc) ++ "</a>|")
    end, Procs),
    ok = file:write(FPID, "</td></tr></tbody></table>"),

    %% Proc info table
    create_table_heading(FPID,Infos),
    lists:foreach(fun
    (Proc) ->
        ok = file:write(FPID, "<tr><td><a href=\"#top\">^ top</a></td><td><a name=\""++ensure_list(Proc)++"\">" ++ ensure_list(Proc) ++ "</a></td>"),
        case rpc:call(N, erlang, process_info, [Proc, Infos]) of
            undefined ->
                ok = file:write(FPID, create_empty_tds(length(Infos)));
            ProcInfos ->

                %% Here we can check the proc info's, and use the bootstrap classes..on TR's
                % .active Applies the hover color to a particular row or cell
                % .success    Indicates a successful or positive action
                % .info   Indicates a neutral informative change or action
                % .warning    Indicates a warning that might need attention
                % .danger Indicates a dangerous or potentially negative action

                lists:foreach(fun
                    (undefined) ->
                        ok;
                    ({backtrace, Info}) ->
                        true = ets:insert(backtraces, {Proc, Info}),
                        % ok = file:write(FPID, lists:flatten("<td>" ++ io_lib:format("~50s", [ensure_list(Info)]) ++ "</td>"));
                        ok = file:write(FPID, "<td><a href=\"#"++ensure_list(Proc)++"_bt\">Backtrace</a></td>");
                    ({_InfoType, Info}) ->
                        ok = file:write(FPID, "<td>" ++ pretty_format(ensure_list(Info)) ++ "</td>")
                end, ProcInfos)
        end,
        ok = file:write(FPID, "<td><a href=\"#top\">^ top</a></td></tr>")
    end, Procs),
    ok = file:write(FPID, "</tbody></table>"),

    %% Back traces table
    ok = file:write(FPID, "<table class=\"table-hover table-bordered\" style=\"table-layout: fixed; width: 100%\" >
        <caption>Backtraces</caption><thead><tr><th>&nbsp;</th><th>Pid</th><th>Backtraces</th></tr></thead><tbody>"),
    lists:foreach(fun({Pid, BT}) -> 
        ok = file:write(FPID, "<tr><td><a href=\"#top\">^ top</a></td><td><a name=\""++ensure_list(Pid)++"_bt\">"++ensure_list(Pid)++"</a></td>
            <td>"++lists:flatten(io_lib:format("~s", [ensure_list(BT)]))++"</td></tr>")
    end, ets:tab2list(backtraces)),
    ok = file:write(FPID, "</tbody></table>"),

    ok = file:write(FPID, create_part2()),
    true = ets:delete(backtraces).

create_part1() ->
    "<!doctype html><html lang=\"en\">
    <head>
      <meta charset=\"utf-8\">
      <title>" ++ atom_to_list(node()) ++ " process infos</title>
      <!--[if lt IE 9]>
      <script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script>
      <![endif]-->
        <!-- Latest compiled and minified CSS -->
        <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css\" integrity=\"sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7\" crossorigin=\"anonymous\">
        <!-- Optional theme -->
        <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css\" integrity=\"sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r\" crossorigin=\"anonymous\">
        <!-- Latest compiled and minified JavaScript -->
        <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js\" integrity=\"sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS\" crossorigin=\"anonymous\"></script>
    </head>
    <body><a name=\"top\"></a>
      <!-- <script src=\"js/scripts.js\"></script> -->
      ".

create_table_heading(FPID,Infos) ->
    ok = file:write(FPID, "<table class=\"table-hover table-bordered\" style=\"white-space: pre-wrap;\" ><captian>Process Infos</captian><thead><tr><th>&nbsp;</th><th>Pid</th>"),
    lists:foreach(fun(I) ->
        ok = file:write(FPID,"<th>"++ensure_list(I)++"</th>")
    end, Infos),
    ok = file:write(FPID, "<th>&nbsp;</th></tr></thead><tbody>").

pretty_format(T) ->
    % io_lib:format("~s",[T]).
    io_lib:format("~p",[T]).

create_part2() ->
   "
   </body>
    </html>".

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

create_empty_tds(0) ->
    "";
create_empty_tds(X) when X > 0 ->
    ["<td>&nbsp;</td>"|create_empty_tds(X-1)].

