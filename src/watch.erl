-module(watch).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0]).
-export([is_on_code_path/1]).
-export([code_path_dirs_without_libs/0]).
-export([add_file_changed_action/1]).
-export([update_file_changed_action/1]).
-export([remove_file_changed_action/1]).
-export([get_file_changed_actions/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(filename, [basename/1, rootname/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-define(ENV_WATCH_DIRS, "WATCH_DIRS").

-record(state, {
          app,
          ports = [],
          watch_dirs = [],
          checksums = [],
          file_changed_buffer = dict:new(),
          last_change_now_timestamp = erlang:now(),
          file_changed_actions = []
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

update_file_changed_action({Id, Fun}) when is_atom(Id), is_function(Fun) ->
    gen_server:call(?SERVER, {update_file_changed_action, {Id, Fun}}).

add_file_changed_action({Id, Fun}) when is_atom(Id), is_function(Fun) ->
    gen_server:call(?SERVER, {add_file_changed_action, {Id, Fun}}).

remove_file_changed_action(Id) ->
    gen_server:call(?SERVER, {remove_file_changed_action, Id}).

get_file_changed_actions() ->
    gen_server:call(?SERVER, get_file_changed_actions).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, App} = application:get_application(?MODULE),
    {ok, Cwd} = file:get_cwd(),
    WatchDirsStr = getenv(?ENV_WATCH_DIRS, Cwd),
    Dirs = get_watch_dirs(WatchDirsStr),
    Ports = [fswatch:start_port(Dir) || Dir <- Dirs],
    tick(),
    {ok, #state{app = App, watch_dirs= Dirs, ports = Ports}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call({add_file_changed_action, Action}, _From, #state{file_changed_actions = FCA} = State) ->
    FCA1 = [Action|FCA],
    {reply, ok, State#state{file_changed_actions = FCA1}};

handle_call({update_file_changed_action, {Id, _} = Action}, _From, #state{file_changed_actions = FCA} = State) ->
    FCA1 = lists:keydelete(Id, 1, FCA),
    FCA2 = [Action|FCA1],
    {reply, ok, State#state{file_changed_actions = FCA2}};

handle_call({remove_file_changed_action, Id}, _From, #state{file_changed_actions = FCA} = State) ->
    FCA1 = lists:keydelete(Id, 1, FCA),
    {reply, ok, State#state{file_changed_actions = FCA1}};

handle_call(get_file_changed_actions, _From, #state{file_changed_actions = FCA} = State) ->
    {reply, FCA, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, State};

handle_info(tick, #state{file_changed_buffer = FCB} = State) ->
    tick(),
    LastChangeNow = State#state.last_change_now_timestamp,
    Now = erlang:now(),
    State1 =
        case {dict:size(FCB) > 0, elapsed_time_msecs(Now, LastChangeNow) > 500} of
            {true, true} ->
                State0 = handle_file_changed(State),
                State0#state{last_change_now_timestamp = Now};
            {_, _} ->
                State
        end,
    {noreply, State1};

handle_info({_Port, {data, {eol, RawDataString}}}, State) ->
    [Path0|Flags0] = parse_raw_input(RawDataString),
    State1 =
        case is_ignored_file(Path0) of
            true ->
                watch_log:debug("ignored ~p ", [Path0]),
                State;
            false ->
                maybe_reload(Path0),
                FCB = dict:store(Path0, Flags0, State#state.file_changed_buffer),
                State#state{file_changed_buffer = FCB}
        end,
    {noreply, State1}.


terminate(Reason, #state{ports = Ports}) ->
    [begin
         try
             {os_pid, OsPid} = erlang:port_info(Port, os_pid),
             Cmd = "kill -9 " ++ integer_to_list(OsPid),
             erlang:port_close(Port),
             os:cmd(Cmd),
             ok
         catch Error:Reason ->
                 watch_log:info("ERROR failed to stop port ~p ~p", [Error, Reason])
         end
     end || Port <- Ports],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
tick() ->
    erlang:send_after(100, ?MODULE, tick).

handle_file_changed(State) ->
    FCB = State#state.file_changed_buffer,
    watch_log:debug("running files chanaged for ~p", [dict:to_list(FCB)]),
    State1 = lists:foldl(fun({Path, Flags}, State0) ->
                                 maybe_file_changed(State0, Path, Flags)
                         end,
                         State,
                         [{Path, Flags} || {Path, Flags} <- dict:to_list(FCB)]),
    State1#state{file_changed_buffer = dict:new()}.

is_ignored_file(Path) ->
    case {is_flymake_file(Path), is_beam_file(Path), is_erl_file(Path)} of
        {false, true, false} -> false;
        {false, false, true} -> false;
        _ -> true
    end.

parse_raw_input(RawDataString) ->
    string:tokens(RawDataString, " ").

elapsed_time_msecs(Now, LastChangeNow) ->
    timer:now_diff(Now, LastChangeNow) div 1000.

maybe_file_changed(State, Path, Flags) ->
    maybe_wait_for_flymake(State#state.watch_dirs),
    file_changed(State, Path, Flags).

maybe_wait_for_flymake(WatchDirs) ->
    case is_flymake_still_working(WatchDirs) of
        true ->
            timer:sleep(500);
        false ->
            ok
    end.

%% Only reload files that are explicitly added to the code path e.g.
%% using -pa
is_on_code_path(Path) ->
    FileName = filename:basename(Path),
    length([1 || Dir <- code_path_dirs_without_libs(), filelib:is_file(Dir ++ FileName)]) > 0.


code_path_dirs_without_libs() ->
    [maybe_add_trailing_slash(maybe_remove_trailing_dot(filename:absname(Dir)))
     || Dir <- code:get_path(),
        not is_string_contains(filename:absname(Dir),
                               filename:absname(code:lib_dir()))].


maybe_add_trailing_slash(Dir) ->
    case lists:last(Dir) of
        $/ ->
            Dir;
        _ ->
            Dir ++ "/"
    end.


maybe_remove_trailing_dot(Dir) ->
    case lists:last(Dir) of
        $. ->
            string:strip(Dir, right, $.);
        _ ->
            Dir
    end.


is_string_contains(String, Substring) ->
    string:rstr(String, Substring) > 0.


getenv(VarName, Default) ->
    case os:getenv(VarName) of
        false ->
            Default;
        Value ->
            Value
    end.

get_watch_dirs(WatchDirsStr) ->
    string:tokens(WatchDirsStr, ":").


file_changed(State, Path, _Flags) ->
    case is_file_content_changed(Path, State) of
        {true, MD5Sum} ->
            file_changed_actions(Path, State#state.file_changed_actions),
            update_path_state(Path, State, MD5Sum);
        false ->
            State
    end.

is_flymake_still_working(WatchDirs) ->
    is_flymake_still_working(WatchDirs, false).


is_flymake_still_working(_, true) ->
    true;
is_flymake_still_working([], Result) ->
    Result;
is_flymake_still_working([Dir|Rest], false) ->
    Result = length(filelib:wildcard(Dir ++ "/**_flymake.beam")) > 0,
    is_flymake_still_working(Rest, Result).

is_file_content_changed(Path, State) ->
    MD5Sum = md5sum(Path),
    case get_path_from_state(Path, State) of
        {Path, PrevMD5Sum} when PrevMD5Sum == MD5Sum ->
            watch_log:debug("path ~p touched but checksum same ~p ~p ~n", [Path, PrevMD5Sum, MD5Sum]),
            false;
        {Path, _PrevMD5Sum} ->
            watch_log:debug("path ~p changed ~n", [Path]),
            {true, MD5Sum};
        false ->
            watch_log:debug("patch ~p changed (no previous checksum) ~n", [Path]),
            {true, MD5Sum}
    end.

is_beam_file(Path) ->
    filename:extension(Path) == ".beam".

is_erl_file(Path) ->
    filename:extension(Path) == ".erl".


is_flymake_file(Path) ->
    is_string_contains(Path, "_flymake.").

get_path_from_state(Path, State) ->
    lists:keyfind(Path, 1, State#state.checksums).

update_path_state(Path, State, MD5Sum) ->
    Checksums = lists:keystore(Path, 1, State#state.checksums, {Path, MD5Sum}),
    State#state{checksums = Checksums}.


maybe_reload(Path) ->
    case is_beam_file(Path) andalso is_on_code_path(Path) of
        true ->
            Module = module_name(Path),
            code:purge(Module),
            code:load_file(Module);
        false ->
            ok
    end.

md5sum(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            erlang:md5(Data);
        {error, _Reason} ->
            <<>>
    end.

module_name(Filename) ->
    list_to_atom(rootname(basename(Filename))).

file_changed_actions(Path, FileChangedActiond) when FileChangedActiond == [] ->
    watch_log:debug("no file changed actions for ~p ~n", [Path]),
    ok;
file_changed_actions(Path, FileChangedActiond) ->
    lists:foreach(
      fun({Id, Action}) ->
              watch_log:debug("file changed action ~p for path ~p ~n", [Id, Path]),
              try Action(Path) of
                  _ -> ok
              catch Error:Reason ->
                      watch_log:info("file changed action failed ~p ~p~n", [Error, Reason]),
                      [watch_log:info(lists:flatten(Line), []) ||
                          Line <- erl_util:pretty_print_stacktrace(erlang:get_stacktrace())]

              end
      end, FileChangedActiond).
