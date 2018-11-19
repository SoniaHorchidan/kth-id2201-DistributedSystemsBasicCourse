-module(lamport).
-compile(export_all).


zero() -> 
	0.


inc(Name, T) ->
	T + 1.


merge(Ti, Tj) ->
	max(Ti, Tj).


leq(Ti, Tj) ->
	Ti =< Tj.


clock(Nodes) ->
	[{Node, zero()} || Node <- Nodes].


update(Node, Time, Clock) ->
	List = lists:keyreplace(Node, 1, Clock, {Node, Time}),
	lists:keysort(2, List).


safe(Time, [{_, T} | _]) ->
	leq(Time, T).


sortMessages({_, Time1, _}, {_, Time2, _}) ->
	leq(Time1, Time2).