-module(key).
-compile(export_all).


generate() ->
	random:uniform(1000000000).


between(Key, From, To) ->
	if
		From < To ->
			(From < Key) and (Key =< To);
		From == To ->
			true;
		true ->
			(From =< Key) or (Key < To)
	end.
