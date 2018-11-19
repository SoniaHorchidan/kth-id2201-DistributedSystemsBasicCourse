-module(vect).
-compile(export_all).


zero() ->
	[].


inc(Name, Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, NameTime} ->
			lists:keyreplace(Name, 1, Time, {Name, NameTime + 1});
		false ->
			[{Name, 1} | Time]
	end.


merge([], Time) ->
	Time;
merge([{Name, Ti}|Rest], Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
			[{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
		false ->
			[{Name, Ti} | merge(Rest, Time)]
	end.


leq([], _) ->
	true;
leq([{Name, Ti} | Rest], Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
			if
				Ti =< Tj ->
					leq(Rest, Time);
				true ->
					false
			end;
		false ->
			false
	end.


clock(_) ->
	[].


update(From, Time, Clock) ->
	{From, FromTime} = lists:keyfind(From, 1, Time),
	case lists:keyfind(From, 1, Clock) of
		{From, _} ->
			lists:keyreplace(From, 1, Clock, {From, FromTime});
		false ->
			[{From, FromTime}| Clock]
	end.



safe(Entries, Clock) ->
	leq(Entries, Clock).


sortMessages({_, Time1, _}, {_, Time2, _}) ->
	leq(Time1, Time2).