-module(liveconfig).

% exports for a stand-alone mochiweb-capable instance
-export([start/0]).

-export([launch_watcher/3]).

start() ->
  application:start(liveconfig).

%%%----------------------------------------------------------------------
%%% config slurping and reading
%%%----------------------------------------------------------------------

launch_watcher(Directory, Wildcard, Fun) ->
  live_config_sup:start_watcher(Directory, Wildcard, Fun).
