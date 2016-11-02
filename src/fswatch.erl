-module(fswatch).

-export([find_executable/0, start_port/1]).

find_executable() ->
    os:find_executable("fswatch").

start_port(Path) ->
    erlang:open_port({spawn_executable, fswatch:find_executable()},
        [stream, exit_status, {line, 16384}, {args, ["--event-flags", "--monitor=fsevents_monitor", Path]}, {cd, Path}]).
