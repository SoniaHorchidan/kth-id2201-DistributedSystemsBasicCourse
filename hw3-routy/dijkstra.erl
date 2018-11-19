-module(dijkstra).
-export([table/2, route/2]).


entry(_, []) -> 
	0;
entry(Node, [{Node, Num, _} | _]) ->
	Num;
entry(Node, [_ | Tail]) ->
	entry(Node, Tail).


replace(Node, N, Gateway, Sorted) ->
	Found = lists:keyfind(Node, 1, Sorted),
	case Found of
		false -> 
			lists:keysort(2, Sorted ++ [{Node, N, Gateway}]);
		_ -> 
			lists:keysort(2, lists:keydelete(Node, 1, Sorted) ++ [{Node, N, Gateway}])
	end.


update(Node, N, Gateway, Sorted) ->
	Hops = entry(Node, Sorted),
	case Hops of 
		0 ->
			Sorted;
		_ -> 
			if 
				Hops > N ->
					replace(Node, N, Gateway, Sorted);
				true -> 
					Sorted
			end
	end.


iterate([], _, Table) ->
	Table;
iterate([{_, inf, _} | _], _, Table) ->
	Table;
iterate([{To, Num, By} | Tail], Map, Table) ->
	ReachList = map:reachable(To, Map),
	case ReachList of
		[] ->
			NewList = Tail;
		_ ->
			NewList = updateAll(ReachList, Num + 1, By, Tail)
	end,
	iterate(NewList, Map, Table ++ [{To, By}]).

updateAll([], _, _, Tail) ->
	Tail;
updateAll([H | T], Num, By, Tail) ->
	NewList = update(H, Num, By, Tail),
	updateAll(T, Num, By, NewList).


table(Gateways, Map) ->
	Nodes = map:all_nodes(Map),
	InitialSortedList = construct_dummy_list(Nodes, Gateways, []),
	iterate(InitialSortedList, Map, []).


construct_dummy_list([], [], Sorted) ->
	Sorted;
construct_dummy_list([], [H | T], Sorted) ->
	construct_dummy_list([], T, replace(H, 0, H, Sorted));
construct_dummy_list([H | T], Nodes, Sorted) ->
	construct_dummy_list(T, Nodes, replace(H, inf, unknown, Sorted)).


route(_, []) ->
	notfound;
route(Node, [{Node, Gateway} | _]) ->
	{ok, Gateway};
route(Node, [_ | Tail]) ->
	route(Node, Tail).