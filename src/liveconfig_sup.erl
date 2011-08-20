-module(liveconfig_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-export([start_watcher/3, start_watcher/4]).

start_watcher(Directory, Wildcard, Fun) when is_function(Fun) ->
  supervisor:start_child(?MODULE, [Directory, Wildcard, Fun]).

start_watcher(Directory, Wildcard, Fun, Interval) when is_function(Fun) ->
  supervisor:start_child(?MODULE, [Directory, Wildcard, Fun, Interval]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Watcher = {liveconfig_server,
             {liveconfig_server, start_link, []},
              permanent, 5000, worker, [liveconfig_server]},

  Processes = [Watcher],
  Strategy = {simple_one_for_one, 10, 10},

  {ok,
   {Strategy, lists:flatten(Processes)}}.
