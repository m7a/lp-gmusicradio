#!/usr/bin/env escript
%% -*- erlang -*-
-mode(compile).
-include_lib("stdlib/include/ms_transform.hrl").
-record(song, {idx, path, description, lastplay, playcount, rating}).

main([]) ->
	io:fwrite("Ma_Sys.ma gmusicradio - print albums 1.0.0 (c) " ++
					"2024 Ma_Sys.ma <info@masysma.net>~n"),
	Conf = main_read_config(),
	database_read(maps:get(gmbrc, Conf), maps:get(default_rating, Conf)),
	print_songs(),
	ets:delete(gmusicradio_songs).

main_read_config() ->
	{ok, UserHome} = init:get_argument(home),
	ConfFile = filename:join(UserHome, ".mdvl/gmusicradio.xml"),
	ConfDefault = #{gmbrc => filename:join([UserHome, ".config",
			"gmusicbrowser", "gmbrc"]), default_rating  => 60},
	case file_exists(ConfFile) of
	true ->
		{Element, _} = xmerl_scan:file(ConfFile),
		{gmusicradio, NewConf, _} = xmerl_lib:simplify_element(Element),
		maps:map(fun({Key, OldValue}) ->
			case proplists:get_value(Key, NewConf) of
			undefined ->
				OldValue;
			Value ->
				if
				is_integer(OldValue) -> list_to_integer(Value);
				is_float(OldValue)   -> list_to_float(Value);
				true                 -> Value
				end
			end
		end, ConfDefault);
	false ->
		ConfDefault
	end.

file_exists(File) ->
	case file:read_file_info(File) of
	{ok, _FileInfo} -> true;
	{error, enoent} -> false
	% other cases are special and reported as errors here!
	end.

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
	Idx  = binary_to_integer(database_keyfind(<<"idx">>, L)),
	Path = filename:join(database_keyfind(<<"path">>, L),
					database_keyfind(<<"file">>, L)),
	case file_exists(Path) of
	true ->
		ets:insert(gmusicradio_songs, #song{
			idx=Idx,
			path=Path,
			description=io_lib:format("~s (~s) ~s", [
				database_utf8p(20,
					database_keyfind(<<"artist">>, L)),
				database_utf8p( 4,
					database_keyfind(<<"year">>,   L)),
				database_utf8p(30,
					database_keyfind(<<"album">>,  L))
			]),
			lastplay=binary_to_integer(
					database_keyfind(<<"lastplay">>, L)),
			playcount=binary_to_integer(
					database_keyfind(<<"playcount">>, L)),
			rating=case database_keyfind(<<"rating">>, L) of
				<<>>  -> DefaultRating;
				Value -> binary_to_integer(Value)
			end
		});
	false ->
		ignored
	end.

database_keyfind(Key, List) ->
	{_K, Value} = lists:keyfind(Key, 1, List),
	Value.

database_utf8p(Pad, Str) ->
	SL = string:length(Str),
	case SL > Pad of
	true  -> string:slice(Str, 0, Pad);
	false -> io_lib:format("~s~" ++ integer_to_list(Pad - SL) ++ "s",
								[Str, ""])
	end.

print_songs() ->
	Sorted = lists:usort(lists:map(fun(S) -> S#song.description end,
			ets:select(gmusicradio_songs, ets:fun2ms(
				fun(X) when X#song.rating > 10 -> X end)))),
	lists:foreach(fun(Descr) ->
			io:fwrite("~s~n", [Descr])
		end, Sorted).
