erl-watch
=========

Erlang application to watch and reload changed beam files into an
erlang node. Only loads beam files that are in a directory returned by
`code:get_path()` after removing `code:lib_dir()` applications.

Depends on [fswatch](https://github.com/emcrisostomo/fswatch) for
notifications and therefore scales for large projects.

The environment variable `WATCH_DIRS` a `:` separated list of
directories controls which directories to watch. Changing `WATCH_DIRS`
requires the `watch` application to be restarted.

This application does not watch and recompile changes to sources
because:

- build systems and commands vary wildly between projects.
- recompiling does not need an erlang node.

Using with rebar
----------------

Add as a dependency to application and ensure the
application is started.
[Dynamic Rebar Configuration](https://github.com/rebar/rebar/wiki/Dynamic-configuration)
might be useful here.

Using directly
--------------

* compile using `make compile`
* add ebin directory to your code path
  e.g., `erl -pa .../erl-watch/ebin' -pa ...`
* application:start(watch)

Example Usage
-------------
```sh
cat .../rel/riak/.erlang

%% Adding current projects ebin directories
[code:add_patha(Dep) || Dep <- filelib:wildcard("../../deps/*/ebin")].
[code:add_patha(Dep) || Dep <- filelib:wildcard("../../apps/*/ebin")].
[code:add_patha(Dep) || Dep <- filelib:wildcard("../deps/*/ebin")].
[code:add_patha(Dep) || Dep <- filelib:wildcard("../apps/*/ebin")].
[code:add_patha(Dep) || Dep <- filelib:wildcard("deps/*/ebin")].
[code:add_patha(Dep) || Dep <- filelib:wildcard("apps/*/ebin")].
[code:add_patha(Dep) || Dep <- filelib:wildcard("ebin")].

%% Starting applications
application:start(watch).

WATCH_DIRS=$(pwd) .../rel/riak/bin/riak console # WATCH_DIRS=<dir1>:<dir2> watches <dir1> and <dir2> for changes
```

Suggestion for Auto-recompilation
---------------------------------

I use [watchdog](https://pypi.python.org/pypi/watchdog) like this:

```sh
 CMD='rebar compile'
 watchmedo shell-command \
          --patterns="*.erl;*.hrl;*.dot" \
          --wait \
          --recursive \
          --command="${CMD}"
```