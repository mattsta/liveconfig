-module(liveconfig_server).
-behaviour(gen_server).

-compile(export_all).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/3, start_link/4]).

-type filename() :: string().
-type lastmod()  :: {{tuple(), tuple()}, integer()}.
-type md5()      :: binary().

-record(prev, {filename = ""       :: filename(),
               lastmod  = {-1, -1} :: lastmod(),
               md5      = <<"">>   :: md5()}).

-record(state, {directory           :: string(),
                wildcard            :: string(),
                process_fun         :: function(),
                previous_files = [] :: [#prev{}],
                interval            :: pos_integer()
               }).

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start_link(Directory, Wildcard, Fun) ->
  start_link(Directory, Wildcard, Fun, 5000).  % 5s by default

start_link(Directory, Wildcard, Fun, Interval) ->
  gen_server:start_link(?MODULE, [Directory, Wildcard, Fun, Interval], []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------

init([Directory, Wildcard, Fun, Interval]) ->
  {ok, #state{directory = Directory,
              wildcard = Wildcard,
              process_fun = Fun,
              interval = Interval}, Interval}.

handle_call(_, _From, #state{interval = Interval} = State) ->
  {reply, not_implemented, State, Interval}.

handle_cast(_Msg, #state{interval = Interval} = State) ->
  {noreply, State, Interval}.

% the atom 'timeout' is automatically sent to this process because
% we are using three-sized return values.  The last value is the
% number of milliseconds of no activity until a 'timeout' is generated.
% It's a way of avoiding use of timer:send_after.
handle_info(timeout, #state{directory = Directory,
                            wildcard = Wildcard,
                            process_fun = ProcessFunction,
                            previous_files = PreviousFiles,
                            interval = Interval} = State) ->
  CurrentFilenames = filelib:wildcard(Wildcard, Directory),
  FullNames = [string:join([Directory, Name], "/") || Name <- CurrentFilenames],
  case find_changes(PreviousFiles, FullNames) of
    no_updates -> {noreply, State, Interval};
    {ChangedFilenames, NewFilenames, RemovedFilenames, NewPreviousFiles} ->
      (catch ProcessFunction(ChangedFilenames, NewFilenames, RemovedFilenames)),
      NewState = State#state{previous_files = NewPreviousFiles},
      {noreply, NewState, Interval}
  end;

handle_info(shutdown, State) ->
  {stop, normal, State};

handle_info(_Other, #state{interval = Interval} = State) ->
  {noreply, State, Interval}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%--------------------------------------------------------------------
%%% find what changed
%%%--------------------------------------------------------------------

find_changes(PreviousFiles, CurrentFilenames) ->
  PreviousFilenames = [P#prev.filename || P <- PreviousFiles],

  CurrentNames = sets:from_list(CurrentFilenames),
  PreviousNames = sets:from_list(PreviousFilenames),

  % NewFiles exist in current, but not in previous
  NewFilenames = sets:to_list(sets:subtract(CurrentNames, PreviousNames)),
  % RemovedFiles exist in previous, but not in current
  RemovedFilenames = sets:to_list(sets:subtract(PreviousNames, CurrentNames)),

  % files to check existed previously and now (otherwise, => removed or new)
  CheckFiles = sets:to_list(sets:intersection(CurrentNames, PreviousNames)),

  ChangedResults =
    [file_changed(lists:keyfind(Current, 2, PreviousFiles), Current) ||
      Current <- CheckFiles],

  %  NotChanged = [NotChanged || {NotChanged, false} <- ChangedResults],

  % ChangedResults is a list of either {Name, false} or {Name, #prev{}}
  {UpdatedFiles, ChangedFilenames} =
    lists:foldl(fun({Name, #prev{filename = Name} = Updated}, {Files, Names}) ->
                  {[Updated | Files], [Name | Names]};
                ({_, false}, A) ->
                  A
                end,
                {[], []},
                ChangedResults),

%  The foldl above replaces these two list comprehensions.  Not sure which
%  is better.  Need to benchmark them.  At least the foldl doesn't iterate
%  over the same contents more than once?
%  UpdatedFiles = [Updated || {_, #prev{} = Updated} <- ChangedResults],
%  ChangedFilenames = [N || #prev{filename = N} <- UpdatedFiles],

  case {NewFilenames, RemovedFilenames, UpdatedFiles} of
    {[], [], []} -> no_updates;
               _ -> NewPreviousFiles =
                      update_previous_files(PreviousFiles, NewFilenames,
                                            RemovedFilenames, UpdatedFiles),
                    {ChangedFilenames, NewFilenames,
                     RemovedFilenames, NewPreviousFiles}
  end.


% false = file isn't changed.  #prev{} = file changed, here's the new metadata.
-spec file_changed(#prev{}, filename()) -> false | #prev{}.
file_changed(#prev{filename = NewFilename,
                   lastmod = {OldMod, OldSz},
                   md5 = OldMd5},
             NewFilename) ->
  Changed =
  case {filelib:last_modified(NewFilename), filelib:file_size(NewFilename)} of
    {OldMod, OldSz} -> false;
    {NewMod, NewSz} -> case file:read_file(NewFilename) of
                         {ok, Data} -> case erlang:md5(Data) of
                                         OldMd5 -> false;
                                         NewMd5 ->
                                           #prev{filename = NewFilename,
                                                 lastmod = {NewMod, NewSz},
                                                 md5 = NewMd5}
                                       end;
                                  _ -> false % read failure is non-update
                       end
  end,
  {NewFilename, Changed};  % returned tagged so we can reference if just 'false'
file_changed(_, NewFilename) ->
  file_changed(#prev{filename = NewFilename}, NewFilename).


-spec update_previous_files([#prev{}],
                            [filename()], [filename()],
                            [#prev{}]) -> [#prev{}].
update_previous_files(PreviousFiles, NewFilenames,
                      RemovedFilenames, UpdatedFiles) ->

  Removed = lists:foldl(fun(E, A) ->
                          lists:keydelete(E, 2, A)
                        end,
                        PreviousFiles,
                        RemovedFilenames),

  Added = lists:foldl(fun(E, A) ->
                        % file_changed with empty #prev{} will populate new rec
                        {E, PrevChanged} = file_changed(update, E),
                        [PrevChanged | A]
                      end,
                      Removed,
                      NewFilenames),

  Updated = lists:foldl(fun(#prev{filename = Name} = Prev, A) ->
                          lists:keyreplace(Name, 2, A, Prev)
                        end,
                        Added,
                        UpdatedFiles),

  Updated.
