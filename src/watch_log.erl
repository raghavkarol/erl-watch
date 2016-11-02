-module(watch_log).

-export([info/2, debug/2, is_valid_log_level/1]).

is_valid_log_level(Level) ->
    log_level(Level) >= 0.

info(Format, Args) ->
    do_log(info, "INFO " ++ Format, Args).


debug(Format, Args) ->
    do_log(debug, "DEBUG " ++ Format, Args).



do_log(Level, Format, Args) ->
    case is_log_enabled(Level) of
        true ->
            io:format(maybe_add_newline(Format), Args);
        _ ->
            ok
    end.

%% Internal Functions
log_level(off) -> 0;
log_level(info) -> 1;
log_level(debug) -> 2.

maybe_add_newline(String) ->
    case is_trailing_newline(string:strip(String)) of
        true ->
            String;
        false ->
            String ++ "~n"
    end.

is_trailing_newline([]) ->
    false;
is_trailing_newline([$~, $n]) ->
    true;
is_trailing_newline([_|Rest]) ->
    is_trailing_newline(Rest).

is_log_enabled(Level) ->
    log_level(Level) =< log_level(watch_app:log_level()).
