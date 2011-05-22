%% $ erlc serial.erl
%% $ erl -noshell -run serial start <PATH>

-module(serial).

-export([start/1]).

%% @doc Start function.
%% @spec start(Path) -> void()
start(Path) ->
    %% Open file
    {ok, File} = file:open(Path, [raw, binary, read_ahead]),

    %% Scan file
    Count = scan_file(File, 0),

    %% Print result
    io:format("~B~n", [Count]),
    init:stop().

%% @doc Scans a file.
%% @spec scan_file(File, Count) -> integer()
scan_file(File, Count) ->
    case file:read_line(File) of
        {ok, _Data} ->
            NewCount = scan_file(File, Count + 1),
            NewCount;
        eof ->
            Count
    end.
