-module(watch_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([log_level/0, log_level/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    watch_sup:start_link().

stop(_State) ->
    ok.

log_level(Level) ->
    true = watch_log:is_valid_log_level(Level),
    application:set_env(watch, log_level, Level).

log_level() ->
    application:get_env(watch, log_level, info).
