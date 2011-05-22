%% $ erlc parallel.erl
%% $ erl -noshell -run parallel start PATH

-module(parallel).

-export([start/1]).

-include_lib("kernel/include/file.hrl").

-define(BUFFER_SIZE, 1000000).

-define(NUM_PROCS, 8).

-define(NEWLINE, <<10>>).


%% @doc Start function.
%% @spec start(Path) -> void()
start(Path) ->
    %% Retrieve specific file information
    {ok, FileInfo} = file:read_file_info(Path),

    %% Calculate chunk size in bytes
    FileSize = FileInfo#file_info.size,
    ChunkSize = FileSize div ?NUM_PROCS,
    error_logger:info_msg("~p ~B bytes, ~B bytes/chunk~n",
                          [self(), FileSize, ChunkSize]),

    %% Open file
    Options = [raw, binary, {read_ahead, ?BUFFER_SIZE}],
    {ok, File} = file:open(Path, Options),

    %% Calculate proper offsets
    Offsets = find_newline_offsets(
        File,
        [ I * ChunkSize || I <- lists:seq(1, ?NUM_PROCS - 1) ],
        []),

    %% Since every child process opens the file separately, we can close it
    file:close(File),

    Args = lists:flatten([0, Offsets, FileSize + 1]),

    Parent = self(),

    lists:foreach(
        fun(I) -> spawn_link(
            fun() ->
                start_child(Parent, Path, lists:sublist(Args, I, 2))
            end)
        end, lists:seq(1, ?NUM_PROCS)),
 
    %% Wait for results
    Results = wait(?NUM_PROCS),
    error_logger:info_msg("~p ~p~n", [self(), lists:sum(Results)]),

    %% Terminate node
    init:stop().

%% @doc Waits for results.
%% @spec wait(NumProcs) -> integer()
wait(NumProcs) ->
    wait(NumProcs, []).

wait(0, Results) ->
    Results;
wait(NumProcs, Results) ->
    receive
        {ok, Count} ->
            wait(NumProcs - 1, [Count|Results]);
        {error, Reason} ->
            error_logger:error_msg("~p~n", [Reason])
    end.

%% @doc Starts processing data.
%% @spec start_child(Parent, Path, Options::list()) -> void()
start_child(Parent, Path, [Offset, Limit]) ->
    error_logger:info_msg("~p ~s ~B ~B~n", [self(), Path, Offset, Limit]),
    case process_path(Path, Offset, Limit) of
        {ok, Count} ->
            Parent ! {ok, Count};
        {error, Reason} ->
            error_logger:error_msg("~p ~P~n", [self(), Reason])
    end.

%% @doc Finds newline offsets.
%% @spec find_newline_offsets(File, Offsets::list(), Acc::list()) -> list()
find_newline_offsets(_File, [], Acc) ->
    lists:reverse(Acc);
find_newline_offsets(File, Offsets, Acc) ->
    [Offset|Rest] = Offsets,
    NewOffset = find_next_newline(File, Offset),
    find_newline_offsets(File, Rest, [NewOffset|Acc]).

%% @doc Finds next newline from offset.
%% @spec find_next_newline(File, Offset::int()) -> int()
find_next_newline(File, Offset) ->
    {ok, Pos} = file:position(File, Offset),
    {ok, Bin} = file:read(File, ?BUFFER_SIZE),
    {I, 1} = binary:match(Bin, ?NEWLINE),
    Pos + I.

%% @doc Processes file path.
%% @spec process_path(Path, Offset, Limit) -> any()
process_path(Path, Offset, Limit) ->
    Options = [raw, binary, {read_ahead, ?BUFFER_SIZE}],
    {ok, File} = file:open(Path, Options),
    {ok, _} = file:position(File, Offset),
    N = scan_file(File, Limit - Offset - 1, 0),
    {ok, N}.

%% @doc Scans file for newlines.
%% @spec scan_file(File, Limit::integer(), Count::integer) -> integer()
scan_file(_File, Limit, Count) when Limit == 0 ->
    Count;
scan_file(File, Limit, Count) when Limit =< ?BUFFER_SIZE ->
    case file:read(File, Limit) of
        {ok, Bin} ->
            M = binary:matches(Bin, ?NEWLINE),
            Count + length(M);
        eof ->
            Count
    end;
scan_file(File, Limit, Count) ->
    {ok, Bin} = file:read(File, ?BUFFER_SIZE),
    M = binary:matches(Bin, ?NEWLINE),
    scan_file(File, Limit - ?BUFFER_SIZE, Count + length(M)).
