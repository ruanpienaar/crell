-module(crell_web_utils).
-export([ens_bin/1, ens_atom/1, ens_int/1,
         localtime_ms_str/1
    ]).

ens_bin(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V));
ens_bin(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
ens_bin(V) when is_list(V) ->
    list_to_binary(V);
ens_bin(V) when is_binary(V) ->
    V;
ens_bin(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V)).

ens_atom(V) when is_list(V) ->
    list_to_atom(V);
ens_atom(V) when is_binary(V) ->
    list_to_atom(binary_to_list(V)).

ens_int(V) when is_list(V) ->
    list_to_integer(V);
ens_int(V) when is_binary(V) ->
    binary_to_integer(V).

localtime_ms_str(Now) ->
    {{Yy,Mm,Dd}, {Hours, Minutes, Seconds, MicroS}} = localtime_ms(Now),
    io_lib:format("~p-~p-~p ~p:~p:~p.~p", [Yy,Mm,Dd,Hours,Minutes,Seconds,MicroS]).

localtime_ms(Now) ->
    {_, _, Micro} = Now,
    {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    {Date, {Hours, Minutes, Seconds, Micro div 1000 rem 1000}}.