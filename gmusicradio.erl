#!/usr/bin/env escript
%% -*- erlang -*-
-mode(compile).
-include_lib("stdlib/include/ms_transform.hrl").

-record(song, {idx, path, description, lastplay, playcount, rating}).

% TODO ADD COMMENTS TO SECTIONS.
% TODO CONSIDER ADDING LIFECYCLE FEATURE
% TODO UNICODE BUG

%---------------------------------------------------------------[ Entrypoint ]--
main([]) ->
	Conf = main_read_config(),
	database_read(dict:fetch(gmbrc, Conf),
					dict:fetch(default_rating, Conf)),
	main_loop(Conf, playback_init(schedule_compute(Conf)), 1,
			podcast_init(Conf)),
	ets:delete(gmusicradio_songs).

main_read_config() ->
	{ok, UserHome} = init:get_argument(home),
	ConfFile = filename:join(UserHome, ".mdvl/gmusicradio_config.erl"),
	% For podcast_dir  and default_rating the values could be parsed from
	%     podcast_conf and gmbrc respectively. This is more complicated than
	% stating them explicitly here. Hence, the less-complicated explicit
	% implementation is provided here.
	ConfDefault = dict:from_list([
		{podcast_conf, "/data/programs/music2/supplementary/news/conf"},
		{podcast_dir,  "/data/programs/music2/supplementary/news/pod"},
		{gmbrc,        filename:join([UserHome, ".config",
						"gmusicbrowser", "gmbrc"])},
		{podcast_chck,   10},
		{schedule_len,   60},
		{await_ms,       60000},
		{chaos_factor,   2.0},
		{min_good_perc,  30},
		{default_rating, 60}
	]),
	case file:read_file_info(ConfFile) of
	{ok, _FileInfo} ->
		{ok, NewConf} = file:consult(ConfFile),
		dict:map(fun({Key, OldValue}) ->
				case dict:is_key(Key, NewConf) of
				true  -> dict:lookup(Key, NewConf);
				false -> OldValue
				end
			end, ConfDefault);
	{error, _} ->
		ConfDefault
	end.

% Endless loop processing
main_loop(Conf, {finish, Next, none, _}, Ctr, PodcastState) ->
	[H|T] = schedule_compute(Conf),
	main_loop(Conf, {await, Next, H, T}, Ctr + 1, PodcastState);
main_loop(Conf, PlaybackState={await, _, _, _}, Ctr, PodcastState) ->
	timer:sleep(dict:fetch(await_ms, Conf)),
	NewPodcastState = case (Ctr rem dict:fetch(podcast_chck, Conf)) == 0 of
			true  -> podcast_process(PodcastState);
			false -> PodcastState
			end,
	StateNew = playback_continue(PlaybackState),
	main_loop(Conf, StateNew, Ctr + 1, NewPodcastState).

%------------------------------------------------------------[ GMBRC Parsing ]--
% Reads the songs from the GMBRC and stores them in ets table `songs` by idx key

database_read(GMBRC, DefaultRating) ->
	io:fwrite("Read GMBRC... "),
	{ok, RawDataBinary} = file:read_file(GMBRC),
	{await_eof, Lines, _Keys} = lists:foldl(fun database_line/2,
			{await_marker, invalid, invalid},
			binary:split(RawDataBinary, <<"\n">>, [global, trim])),
	ets:new(gmusicradio_songs, [set, named_table, {keypos, #song.idx}]),
	lists:foreach(fun(Line) ->
				database_convert_store(DefaultRating, Line)
			end, Lines),
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

database_convert_store(DefaultRating, L) ->
	Idx = binary_to_integer(database_keyfind(<<"idx">>, L)),
	ets:insert(gmusicradio_songs, #song{
		idx=Idx,
		path=filename:join(database_keyfind(<<"path">>, L),	
					database_keyfind(<<"file">>, L)),
		description=io_lib:format("~ts ~ts ~ts (~ts)", [
			database_utf8p(10, database_keyfind(<<"artist">>, L)),
			database_utf8p(20, database_keyfind(<<"album">>,  L)),
			database_utf8p(20, database_keyfind(<<"title">>,  L)),
			database_utf8p( 4, database_keyfind(<<"year">>,   L))
		]),
		lastplay=binary_to_integer(database_keyfind(<<"lastplay">>, L)),
		playcount=binary_to_integer(
					database_keyfind(<<"playcount">>, L)),
		rating=case database_keyfind(<<"rating">>, L) of
			<<>>  -> DefaultRating;
			Value -> binary_to_integer(Value)
		end
	}).

database_keyfind(Key, List) ->
	{_K, Value} = lists:keyfind(Key, 1, List),
	Value.

% TODO THIS UNICODE PART DOES NOT WORK YET...
database_utf8p(Pad, Binary) ->
	Primary = unicode:characters_to_binary(bitstring_to_list(Binary), utf8),
	Len = string:length(Primary),
	io:fwrite("str=<~p> len=<~p>~n", [Primary, Len]),
	case Len > Pad of
	true ->
		string:slice(Primary, 0, Pad);
	false ->
		[Primary|lists:map(fun(_X) -> " " end, lists:seq(1, Pad - Len))]
	end.

%-----------------------------------------------------[ Schedule Computation ]--
schedule_compute(Conf) ->
	MinGoodPerc = dict:fetch(min_good_perc, Conf),
	ChaosFactor = dict:fetch(chaos_factor,  Conf),
	ScheduleLen = dict:fetch(schedule_len,  Conf),
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
	Duplicate = case Perce4 + Perce5 < MinGoodPerc of
			true  -> trunc(MinGoodPerc / (Perce4 + Perce5));
			false -> 1
			end,
	io:fwrite("Duplicate = ~p~n", [Duplicate]),
	Schedule = schedule_merge([
		schedule_shuffle(ChaosFactor, Group5, Duplicate),
		schedule_shuffle(ChaosFactor, Group4, Duplicate),
		schedule_shuffle(ChaosFactor, Group3, 1),
		schedule_shuffle(ChaosFactor, Group2, 1)
	], ScheduleLen),
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
schedule_shuffle(ChaosFactor, Group, Duplicate) ->
	lists:flatten(lists:map(fun(_Ctr) ->
		[Y || {_, Y} <- lists:sort(
			lists:map(fun(S) ->
					{rand:uniform() * ChaosFactor +
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
	{ok, _Cnt} = playback_enqueue(H1),
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
	playback_enqueue_file(Entry#song.path).

playback_enqueue_file(File) ->
	{ok, _Cnt} = subprocess_run_await(["gmusicbrowser", "-enqueue", File]).

playback_check_is_running(ID) ->
	[Entry] = ets:lookup(gmusicradio_songs, ID),
	io:fwrite("AWAIT   ~ts~n", [Entry#song.description]),
	{ok, RawStatus} = subprocess_run_await(["dbus-send",
			"--print-reply", "--dest=org.gmusicbrowser",
			"/org/gmusicbrowser", "org.gmusicbrowser.CurrentSong"]),
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
		binary:list_to_bin(filename:join(Path, File)) == Entry#song.path
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
podcast_init(Conf) ->
	{Conf, podcast_run_inner(Conf)}.

podcast_run_inner(Conf) ->
	{ok, _Cnt} = subprocess_run_await(["podget", "-d",
					dict:fetch(podcast_conf, Conf)]),
	lists:sort(filelib:wildcard(dict:fetch(podcast_dir, Conf) ++
								"/**/*.mp3")).

podcast_process({Conf, OldState}) ->
	NewState = podcast_run_inner(Conf),
	case NewState -- OldState of
	[] ->
		ok; % do nothing
	NewFiles ->
		PlayFile = lists:last(NewFiles),
		io:fwrite("PODCAST ~ts~n", [PlayFile]),
		playback_enqueue_file(PlayFile)
	end,
	{Conf, NewState}.

%-------------------------------------------------------[ Subprocess Library ]--
% Enables the spawning of a subprocess with checking its output and return code
% and using the shell and space safe way of giving the command as a list of
% parts rather than a shell script...

% https://erlang.org/pipermail/erlang-questions/2007-February/025210.html
subprocess_run_await([ExecutableName|Args]) ->
	subprocess_get_data(open_port(
			{spawn_executable, os:find_executable(ExecutableName)},
			[{args, Args}, stream, exit_status,
					use_stdio, stderr_to_stdout, in]
		), []).

subprocess_get_data(Port, Acc) ->
	receive
	{Port, {data, D}} ->
		subprocess_get_data(Port, [D|Acc]);
	{Port, {exit_status, RC}} ->
		case RC == 0 of
		true  -> {ok, lists:reverse(Acc)};
		false -> {error, RC, lists:reverse(Acc)}
		end
	end.
