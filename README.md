liveconfig: run a function when files in a directory change
===========================================================

What
----
liveconfig watches a directory with a filename glob then runs a callback
when files change (or are added/removed).  Example use case: a web server
automatically reloading its config based on which *.conf files are in a
directory of deployed services.


Usage
-----
        liveconfig:start().
        liveconfig_sup:start_watcher("./ebin", "*.beam",
          fun(Changed, New, Removed) ->
            io:format("Got ~p, ~p, ~p~n", [Changed, New, Removed])
          end, 5000).

You start a `liveconfig_server` by calling `liveconfig_sup:start_watcher/4`
where the arguments are: directory to watch, glob of files to watch,
function to run on changes, and the check interval in milliseconds.

The arguments to your callback function are lists of file names with the
path prepended.  The first argument is any changed files in this interval,
the second argument is new files, and the third argument is files previously
present but now absent.  Your callback function is only invoked if one of the
three parameters is non-empty.

Your callback function is called directly in the `liveconfig_server`
(i.e. not spawned) so you do not have to worry about more than
one callback function running at once.


Performance Notes
-----------------
liveconfig is suited to maintaining a small (under 1000 files) directory
of config files you watch for updates.  You can adjust the check interval
if performance starts to drag.

You can watch a directory of 6 files at 50ms with little performance
impact.  If you try to watch a directory of 6000 files, you probably want to
increase your check interval to a few seconds.


Longer Intervals
----------------
Instead of running short intervals of 500ms to 5s, you could increase
the interval to 12 hours or 24 hours to see which files changed over
longer periods of time.  You can run multiple servers over the same directory
simultaneously to get updates about files in "real time" (5s intervals),
hourly, daily, etc.


Building
--------
Build:
        rebar compile

Testing
-------
Manual test from the project root:

        liveconfig:start().
        liveconfig_sup:start_watcher("./ebin", "*.beam",
          fun(A, B, C) -> io:format("Got ~p, ~p, ~p~n", [A, B, C]) end, 5000).
