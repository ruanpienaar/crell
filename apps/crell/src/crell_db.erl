-module(crell_db).

-export([
    ets_entries_per_page/3,
    mnesia_entries_per_page/3
]).

% Didn't work for pagination, but could be useful in the future.
-export([
    all_ets_keys/3
]).

% Ets table
ets_entries_per_page(Node, Tbl, PageNum) ->
    case rpc:call(Node, ets, first, [Tbl]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        '$end_of_table' ->
            {'$end_of_table', 0, []};
        First ->
            Min = case PageNum of
                1 ->
                    0;
                _ ->
                    (PageNum * 50)-50
            end,

            Max = PageNum * 50,
            ets_entries_per_page(Node, Tbl, {_Pos = 0, _Count = 0, Min, Max}, First, [])
    end.

ets_entries_per_page(Node, Tbl, {_Pos, Count, _Min, Max}, _Key, Entries) when Count >= Max ->
    {rpc:call(Node, ets, info, [Tbl, size]), lists:reverse(Entries)};
ets_entries_per_page(Node, Tbl, {Pos, Count, Min, Max}, Key, Entries) when Pos >= Min andalso Pos =< Max ->
    case rpc:call(Node, ets, lookup, [Tbl, Key]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        [] ->
            {rpc:call(Node, ets, info, [Tbl, size]), []};
        [Entry] ->
            case rpc:call(Node, ets, next, [Tbl, Key]) of
                {badrpc, Reason} ->
                    {badrpc, Reason};
                '$end_of_table' ->
                    {rpc:call(Node, ets, info, [Tbl, size]), lists:reverse([Entry|Entries])};
                NextKey ->
                    ets_entries_per_page(Node, Tbl,{Pos+1, Count+1, Min, Max}, NextKey, [Entry|Entries])
            end
    end;
ets_entries_per_page(Node, Tbl, {Pos, Count, Min, Max}, Key, Entries) ->
    case rpc:call(Node, ets, next, [Tbl, Key]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        '$end_of_table' ->
            {rpc:call(Node, ets, info, [Tbl, size]), lists:reverse(Entries)};
        NextKey ->
            ets_entries_per_page(Node, Tbl,{Pos+1, Count, Min, Max}, NextKey, Entries)
    end.

% Mnesia Table
mnesia_entries_per_page(Node, Tbl, PageNum) ->
    case rpc:call(Node, mnesia, dirty_first, [Tbl]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        '$end_of_table' ->
            {'$end_of_table', 0, []};
        First ->
            Min = case PageNum of
                1 ->
                    0;
                _ ->
                    (PageNum * 50)-50
            end,

            Max = PageNum * 50,
            mnesia_entries_per_page(Node, Tbl, {_Pos = 0, _Count = 0, Min, Max}, First, [])
    end.

mnesia_entries_per_page(Node, Tbl, {_Pos, Count, _Min, Max}, _Key, Entries) when Count >= Max ->
    {rpc:call(Node, mnesia, table_info, [Tbl, size]), lists:reverse(Entries)};
mnesia_entries_per_page(Node, Tbl, {Pos, Count, Min, Max}, Key, Entries) when Pos >= Min andalso Pos =< Max ->
    case rpc:call(Node, mnesia, dirty_read, [Tbl, Key]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        [] ->
            {rpc:call(Node, mnesia, table_info, [Tbl, size]), []};
        [Entry] ->
            case rpc:call(Node, mnesia, dirty_next, [Tbl, Key]) of
                {badrpc, Reason} ->
                    {badrpc, Reason};
                '$end_of_table' ->
                    {rpc:call(Node, mnesia, table_info, [Tbl, size]), lists:reverse([Entry|Entries])};
                NextKey ->
                    mnesia_entries_per_page(Node, Tbl,{Pos+1, Count+1, Min, Max}, NextKey, [Entry|Entries])
            end
    end;
mnesia_entries_per_page(Node, Tbl, {Pos, Count, Min, Max}, Key, Entries) ->
    case rpc:call(Node, mnesia, dirty_next, [Tbl, Key]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        '$end_of_table' ->
            {rpc:call(Node, mnesia, table_info, [Tbl, size]), lists:reverse(Entries)};
        NextKey ->
            mnesia_entries_per_page(Node, Tbl,{Pos+1, Count, Min, Max}, NextKey, Entries)
    end.





%% TODO: will this work with a bag table ?

%% TODO: continue from cursor key, don't do repetitive calls to first and next

%% TODO: move into crell server........
%% NB: 50 is hardcoded here and in the backend as the max per page....

-spec all_ets_keys(node(), Tbl :: term(), CursorKey :: term()) ->
    {NewCursorKey :: term(), TblSize :: non_neg_integer(), Entries :: list(term())} |
    {badrpc, term()}.
all_ets_keys(Node, Tbl, undefined) ->
    case rpc:call(Node, ets, first, [Tbl]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        '$end_of_table' ->
            {'$end_of_table', rpc:call(Node, ets, info, [Tbl, size]), []};
        First ->
            all_ets_keys(Node, Tbl, First)
    end;
all_ets_keys(Node, Tbl, CursorKey) ->
    case rpc:call(Node, ets, lookup, [Tbl, CursorKey]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        [] ->
            {'$end_of_table', rpc:call(Node, ets, info, [Tbl, size]), []};
        [Entry] ->
            case rpc:call(Node, ets, next, [Tbl, CursorKey]) of
                {badrpc, Reason} ->
                    {badrpc, Reason};
                '$end_of_table' ->
                    {'$end_of_table', rpc:call(Node, ets, info, [Tbl, size]), [Entry]};
                NextKey ->
                    all_ets_keys_loop(Node, Tbl, 1, NextKey, [Entry])
            end
    end.

all_ets_keys_loop(Node, Tbl, Count, CursorKey, Entries) when Count == 50 ->
    {CursorKey, rpc:call(Node, ets, info, [Tbl, size]), lists:reverse(Entries)};
all_ets_keys_loop(Node, Tbl, Count, CursorKey, Entries) ->
    case rpc:call(Node, ets, lookup, [Tbl, CursorKey]) of
        {badrpc, Reason} ->
            {badrpc, Reason};
        [] ->
            {'$end_of_table', rpc:call(Node, ets, info, [Tbl, size]), []};
        [Entry] ->
            case rpc:call(Node, ets, next, [Tbl, CursorKey]) of
                {badrpc, Reason} ->
                    {badrpc, Reason};
                '$end_of_table' ->
                    {CursorKey, rpc:call(Node, ets, info, [Tbl, size]), [Entry | Entries]};
                NextKey ->
                    all_ets_keys_loop(Node, Tbl, Count+1, NextKey, [Entry | Entries])
            end
    end.

