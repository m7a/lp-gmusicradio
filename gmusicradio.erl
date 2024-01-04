#!/usr/bin/env escript
%% -*- erlang -*-
-mode(compile).
-include_lib("stdlib/include/ms_transform.hrl").

-record(song, {idx, path, description, lastplay, playcount, rating}).

% TODO X THIS COULD BE USER CONFIG SETTINGS
-define(CHAOS_FACTOR, 2.0).
-define(MIN_GOOD_PERC, 30).

% TODO X PODCAST CONFIG SEEMS TO BE EFFECITVELY THE FOLLOWING
% -> podget config directory
% -> podget configured output directory [could parse from podget config!]

% TODO X FINALLY THE CONFIG FILE `gmbrc` path should be given

% TODO would be nice if we were to use the default as
%      configured in gmusicbrowser
-define(DEFAULT_RATING, 60).

% -> maybe since this is just four parameters we could have defaults and user
%    arguments --chaos-factor= --min-good-perc= --gmbrc=... --podget-config-dir=...
%    [--control-gmusicbrowser-lifecycle]
% -> maybe not because some optional things like gmusicbrwoser executable config
%    come to mind?

%---------------------------------------------------------------[ Entrypoint ]--
main([]) ->
	% TODO PROCESS PER CONFIG FILE OR SOMETHING
	database_read("/home/linux-fan/.config/gmusicbrowser/gmbrc"),
	% TODO LOOP INDEFINITELY AND ALSO INTEGRATE WITH PODCAST
	Schedule = schedule_compute(60),
	playback_blocking(Schedule),
	ets:delete(gmusicradio_songs).

% -- TODO NOTES --
%  - To play a given file `gmusicbrowser -enqueue PATH`
%  - To check what is being played: 
%    dbus-send --print-reply --dest=org.gmusicbrowser /org/gmusicbrowser org.gmusicbrowser.CurrentSong
%    dbus-monitor --profile interface=org.gmusicbrowser,member=SongChanged
% + https://erlang.org/pipermail/erlang-questions/2007-February/025210.html
% emits “SongChanged” line once for each song
% $ dbus-monitor --profile interface=org.gmusicbrowser,member=SongChanged
% #type	timestamp	serial	sender	destination	path	interface	member
% #					in_reply_to
% sig	1704403082.100347	2	org.freedesktop.DBus	:1.25	/org/freedesktop/DBus	org.freedesktop.DBus	NameAcquired
% sig	1704403082.100356	4	org.freedesktop.DBus	:1.25	/org/freedesktop/DBus	org.freedesktop.DBus	NameLost
% sig	1704403326.727144	246	:1.10	<none>	/org/gmusicbrowser	org.gmusicbrowser	SongChanged
% sig	1704403588.375003	251	:1.10	<none>	/org/gmusicbrowser	org.gmusicbrowser	SongChanged
% -- END NOTES --

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
playback_blocking([H1|[H2|ScheduleT]]) ->
	ok = playback_enqueue(H1),
	ok = playback_enqueue(H2),
	ok = playback_await(H1),
	playback_blocking_inner(H2, ScheduleT).

% TODO THIS IS INCOMPLETE BECAUSE IT SHOULD BETTER ENQUEUE THE H1 AND THEN RETURN (WITHOUT AWAITING) SUCH THAT PROCESSING CAN TAKE ITS TIME FOR ONE SONG LENGTH AND ALSO SUBSEQUENT INVOCATIONS WOULD PASS THE RETURN VALUE AS “AWAIT” TO playback_blocking_inner. Effectively, only the first call would go to playback_blocking. This is all moot such as long as we don't know the integration of the podcasting yet...
playback_blocking_inner(Await, [H1|[]]) ->
	ok = playback_await(Await),
	H1;
playback_blocking_inner(Await, [H2|ScheduleT]) ->
	ok = playback_await(Await),
	ok = playback_enqueue(H2),
	playback_blocking_inner(H2, ScheduleT).

playback_enqueue(ID) ->
	[Entry] = ets:lookup(gmusicradio_songs, ID),
	io:fwrite("ENQUEUE ~ts~n", [Entry#song.description]),
	% TODO NO ESCAPING OR RETURNCODE CHECKING WHATSOEVER
	os:cmd(io_lib:format("gmusicbrowser -enqueue ~s", [Entry#song.path])),
	ok.

playback_await(ID) ->
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
		case CMP == Entry#song.path of
		true ->
			io:fwrite("FOUND   ~ts~n", [Entry#song.description]),
			% update playcount
			ets:insert(gmusicradio_songs, Entry#song{
					playcount = Entry#song.playcount + 1}),
			ok;
		false ->
			timer:sleep(60000), % TODO LATER UP THIS TO 1min!
			playback_await(ID)
		end
	end.

playback_get_dict_entry(Entry, []) ->
	notfound;
playback_get_dict_entry(Entry, ["dict entry("|Tail]) ->
	playback_check_dict_key(Entry, Tail);
playback_get_dict_entry(Entry, [Skip|Tail]) ->
	playback_get_dict_entry(Entry, Tail).

playback_check_dict_key(Entry, [KeyLine|[ValueLine|Others]]) ->
	CMP = "string \"" ++ Entry ++ "\"",
	if
	KeyLine == CMP -> string:slice(ValueLine, 8, length(ValueLine) - 9);
	true           -> playback_get_dict_entry(Entry, Others)
	end.

%------------------------------------------------------[ Podcast Interaction ]--
