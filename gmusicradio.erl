#!/usr/bin/env escript
%% -*- erlang -*-
-mode(compile).
-include_lib("stdlib/include/ms_transform.hrl").

-record(song, {idx, path, description, lastplay, playcount, rating}).

% TODO -- X THIS COULD BE USER CONFIG SETTINGS --
-define(CHAOS_FACTOR, 2.0).
-define(MIN_GOOD_PERC, 30).
-define(DEFAULT_SCHEDULE, 60).
-define(AWAIT_SONG_PLAYBACK, 60000).
% TODO TESTING ONLY REVERT TO 10 ONCE OK
-define(PODCAST_EVERY, 3).
% TODO -- END CONFIG SETTINGS --

% TODO would be nice if we were to use the default as
%      configured in gmusicbrowser
-define(DEFAULT_RATING, 60).

%---------------------------------------------------------------[ Entrypoint ]--
main([]) ->
	% TODO PROCESS PER CONFIG FILE OR SOMETHING
	database_read("/home/linux-fan/.config/gmusicbrowser/gmbrc"),
	mainloop(playback_init(schedule_compute(?DEFAULT_SCHEDULE)),
		1,
		% TODO MAKE THIS CONFIGURABLE
		podcast_init("/data/programs/music2/supplementary/news/conf",
			"/data/programs/music2/supplementary/news/pod")),
	ets:delete(gmusicradio_songs).

% Endless loop processing
mainloop({finish, Next, none, _}, Ctr, PodcastState) ->
	[H|T] = schedule_compute(?DEFAULT_SCHEDULE),
	mainloop({await, Next, H, T}, Ctr + 1, PodcastState);
mainloop(PlaybackState={await, _, _, _}, Ctr, PodcastState) ->
	timer:sleep(?AWAIT_SONG_PLAYBACK),
	NewPodcastState = case (Ctr rem ?PODCAST_EVERY) == 0 of
			true  -> podcast_process(PodcastState);
			false -> PodcastState
			end,
	StateNew = playback_continue(PlaybackState),
	mainloop(StateNew, Ctr + 1, NewPodcastState).

% TODO USE Erlang open_port({spawn_executable, BINARY}, [{args, [arg0, arg1, ...]}, {..see link..}])
% + https://erlang.org/pipermail/erlang-questions/2007-February/025210.html
% find it in path, then use these primitives to run a subprocess

%------------------------------------------------------------[ GMBRC Parsing ]--
% Reads the songs from the GMBRC and stores them in ets table `songs` by idx key

database_read(GMBRC) ->
	io:fwrite("Read GMBRC... "),
	{ok, RawDataBinary} = file:read_file(GMBRC),
	{await_eof, Lines, _Keys} = lists:foldl(fun database_line/2,
			{await_marker, invalid, invalid},
			binary:split(RawDataBinary, <<"\n">>, [global, trim])),
	ets:new(gmusicradio_songs, [set, named_table, {keypos, #song.idx}]),
	lists:foreach(fun database_convert_store/1, Lines),
	io:fwrite("OK~n").

database_line(<<"[Songs]">>, {await_marker, invalid, invalid}) ->
	{process_headings, invalid, invalid};
database_line(_Line, {await_marker, invalid, invalid}) ->
	{await_marker, invalid, invalid};
database_line(CSV, {process_headings, invalid, invalid}) ->
	{process_contents, [], [<<"idx">>|
				binary:split(CSV, <<"\t">>, [global, trim])]};
database_line(<<>>, {process_contents, List, Keys}) ->
	{await_eof, List, Keys};
database_line(CSV, {process_contents, Tail, Keys}) ->
	{process_contents, [lists:zip(Keys, binary:split(CSV,
				<<"\t">>, [global, trim]))|Tail], Keys};
database_line(_Line, {await_eof, List, Keys}) ->
	{await_eof, List, Keys}.

database_convert_store(Line) ->
	Idx = binary_to_integer(database_keyfind(<<"idx">>, Line)),
	ets:insert(gmusicradio_songs, #song{
		idx=Idx,
		path=filename:join(database_keyfind(<<"path">>, Line),	
					database_keyfind(<<"file">>, Line)),
		description=io_lib:format("~-10ts ~-20ts ~-20ts (~ts)", [
			% TODO SOME PROBLEM WITH UNICODE ... SOMEWHERE...
			database_keyfind(<<"artist">>, Line),
			database_keyfind(<<"album">>,  Line),
			database_keyfind(<<"title">>,  Line),
			database_keyfind(<<"year">>,   Line)
		]),
		lastplay=binary_to_integer(database_keyfind(<<"lastplay">>,
									Line)),
		playcount=binary_to_integer(database_keyfind(<<"playcount">>,
									Line)),
		rating=case database_keyfind(<<"rating">>, Line) of
			<<>>  -> ?DEFAULT_RATING;
			Value -> binary_to_integer(Value)
		end
	}).

database_keyfind(Key, List) ->
	{_K, Value} = lists:keyfind(Key, 1, List),
	Value.

%-----------------------------------------------------[ Schedule Computation ]--

schedule_compute(Limit) ->
	% unclear why select_count did not return the intended output here?
	Count1 = length(ets:select(gmusicradio_songs,
						schedule_construct_match(0))),
	Group2 = ets:select(gmusicradio_songs, schedule_construct_match(20)),
	Group3 = ets:select(gmusicradio_songs, schedule_construct_match(40)),
	Group4 = ets:select(gmusicradio_songs, schedule_construct_match(60)),
	Group5 = ets:select(gmusicradio_songs, schedule_construct_match(80)),
	Count2 = length(Group2),
	Count3 = length(Group3),
	Count4 = length(Group4),
	Count5 = length(Group5),
	CountT = Count2 + Count3 + Count4 + Count5,
	Perce2 = Count2 * 100 / CountT,
	Perce3 = Count3 * 100 / CountT,
	Perce4 = Count4 * 100 / CountT,
	Perce5 = Count5 * 100 / CountT,
	io:fwrite("Distribution of stars~n 1 Star  ~5w (ignore)~n" ++
			" 2 Stars ~5w (~5.2f%)~n 3 Stars ~5w (~5.2f%)~n" ++
			" 4 Stars ~5w (~5.2f%)~n 5 Stars ~5w (~5.2f%)~n",
			[Count1, Count2, Perce2, Count3, Perce3,
			Count4, Perce4, Count5, Perce5]),
	Duplicate = case Perce4 + Perce5 < ?MIN_GOOD_PERC of
			true  -> trunc(?MIN_GOOD_PERC / (Perce4 + Perce5));
			false -> 1
			end,
	io:fwrite("Duplicate = ~p~n", [Duplicate]),
	Schedule = schedule_merge([
		schedule_shuffle(Group5, Duplicate),
		schedule_shuffle(Group4, Duplicate),
		schedule_shuffle(Group3, 1),
		schedule_shuffle(Group2, 1)
	], Limit),
	io:fwrite("Schedule of ~w songs:~n", [length(Schedule)]),
	lists:foreach(fun(ID) ->
			[Entry] = ets:lookup(gmusicradio_songs, ID),
			io:fwrite("I~5w R~w C~5w ~ts~n", [Entry#song.idx,
				Entry#song.rating, Entry#song.playcount,
				Entry#song.description])
		end, Schedule),
	Schedule.

schedule_construct_match(Rating) ->
	ets:fun2ms(fun(X) when X#song.rating > Rating andalso
				X#song.rating =< (Rating + 20) -> X end).

% https://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-
schedule_shuffle(Group, Duplicate) ->
	lists:flatten(lists:map(fun(_Ctr) ->
		[Y || {_, Y} <- lists:sort(
			lists:map(fun(S) ->
					{rand:uniform() * ?CHAOS_FACTOR +
						S#song.playcount, S#song.idx}
				end, Group)
		)]
	end, lists:seq(1, Duplicate))).

schedule_merge(Groups, Limit) ->
	NonEmptyGroups = lists:filter(fun (X) -> X /= [] end, Groups),
	schedule_merge_annotated([], Limit, lists:zipwith(fun(Group, ID) ->
			{ 0.0, ID, 0, length(Group), Group }
		end, NonEmptyGroups, lists:seq(1, length(NonEmptyGroups)))).

schedule_merge_annotated(Schedule, _Limit, []) ->
	lists:reverse(Schedule);
schedule_merge_annotated(Schedule, Limit, _AnnotatedGroups)
					when length(Schedule) >= Limit ->
	lists:reverse(Schedule);
schedule_merge_annotated(Schedule, Limit, AnnotatedGroups) ->
	{_Perc, SelID, Num, Of, [SelItem|SelRem]} = lists:min(AnnotatedGroups),
	Others = lists:filter(fun({_Perc2, ID, _Num, _Of, _Group}) ->
					ID /= SelID end, AnnotatedGroups),
	NewNum = Num + 1,
	NewPerc = NewNum / Of,
	case NewNum < Of of
	true  -> schedule_merge_annotated([SelItem|Schedule], Limit,
			[{NewPerc, SelID, NewNum, Of, SelRem}|Others]);
	false -> schedule_merge_annotated([SelItem|Schedule], Limit, Others)
	end.

%-----------------------------------------------------[ Playback Integration ]--
playback_init([H1|[H2|ScheduleT]]) ->
	ok = playback_enqueue(H1),
	%ok = playback_enqueue(H2), % TODO TEST
	{await, H1, H2, ScheduleT}.

playback_continue({await, Await, Next, Sched}) ->
	case playback_check_is_running(Await) of
	true ->
		[Entry] = ets:lookup(gmusicradio_songs, Await),
		io:fwrite("FOUND   ~ts~n", [Entry#song.description]),
		% update playcount
		ets:insert(gmusicradio_songs, Entry#song{
					playcount = Entry#song.playcount + 1}),
		playback_enqueue(Next),
		case Sched of
		[]    -> {finish, Next, none, []};
		[H|T] -> {await,  Next, H,    T}
		end;
	false ->
		{await, Await, Next, Sched}
	end.

playback_enqueue(ID) ->
	[Entry] = ets:lookup(gmusicradio_songs, ID),
	io:fwrite("ENQUEUE ~ts~n", [Entry#song.description]),
	% TODO NO ESCAPING OR RETURNCODE CHECKING WHATSOEVER
	os:cmd(io_lib:format("gmusicbrowser -enqueue ~s", [Entry#song.path])),
	ok.

playback_check_is_running(ID) ->
	[Entry] = ets:lookup(gmusicradio_songs, ID),
	io:fwrite("AWAIT   ~ts~n", [Entry#song.description]),
	RawStatus = os:cmd("dbus-send --print-reply " ++
			"--dest=org.gmusicbrowser " ++
			"/org/gmusicbrowser org.gmusicbrowser.CurrentSong"),
	% path, file is of interest
	Lines = lists:map(fun string:trim/1,
					string:split(RawStatus, "\n", all)),
	Path = playback_get_dict_entry("path", Lines),
	File = playback_get_dict_entry("file", Lines),
	case Path == notfound orelse File == notfound of
	true ->
		io:fwrite("DBUS communication failed. `path` or `file` " ++
					"absent from output:~n~p~n", [Lines]),
		error;
	false ->
		CMP = binary:list_to_bin(filename:join(Path, File)),
		CMP == Entry#song.path
	end.

playback_get_dict_entry(_Entry, []) ->
	notfound;
playback_get_dict_entry(Entry, ["dict entry("|Tail]) ->
	playback_check_dict_key(Entry, Tail);
playback_get_dict_entry(Entry, [_Skip|Tail]) ->
	playback_get_dict_entry(Entry, Tail).

playback_check_dict_key(Entry, [KeyLine|[ValueLine|Others]]) ->
	CMP = "string \"" ++ Entry ++ "\"",
	if
	KeyLine == CMP -> string:slice(ValueLine, 8, length(ValueLine) - 9);
	true           -> playback_get_dict_entry(Entry, Others)
	end.

%------------------------------------------------------[ Podcast Interaction ]--

podcast_init(ConfigDir, OutputDir) ->
	% TODO SUBPROCESS NO EXIT CODE HANDLING AND NO SPACES SUPPORT
	os:cmd("podget -d " ++ ConfigDir),
	{ConfigDir, OutputDir, lists:sort(filelib:wildcard(OutputDir ++ "/**/*.mp3"))}.

podcast_process({ConfigDir, OutputDir, OldState}) ->
	os:cmd("podget -d " ++ ConfigDir), % TODO SUBPROCESS WARNING
	NewState = lists:sort(filelib:wildcard(OutputDir ++ "/**/*.mp3")),
	case NewState -- OldState of
	[] -> ok; % do nothing
	NewFiles ->
		PlayFile = lists:last(NewFiles),
		io:fwrite("New podcast entries: ~p~n", [PlayFile])
	end,
	{ConfigDir, OutputDir, NewState}.
