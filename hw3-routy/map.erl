-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).


new() -> 
	[].


update(Node,Links,Map) ->
	Tmp = lists:keydelete(Node, 1, Map),	
	[{Node,Links} | Tmp].


reachable(Node, []) ->
	[];
reachable(Node, [{Node, Destinantion} | _]) ->
	Destinantion;
reachable(Node, [_ | Tail])->
	reachable(Node, Tail).


all_nodes(Map) ->
	List = make_list(Map, []),
	remove_dups(flatten(List)).


make_list([], Nodes) ->
	Nodes;
make_list([Head | Tail], Nodes) ->
	make_list(Tail, Nodes ++ tuple_to_list(Head)).


append([H | T], L) -> [H | append(T, L)];
append([], L) -> L.


flatten([[_|_]=H|T]) ->
	 append(flatten(H), flatten(T));
flatten([[]|T]) -> 
	flatten(T);
flatten([H|T]) ->
	[H|flatten(T)];
flatten([]) -> [].


remove_dups([]) -> [];
remove_dups([H|T]) ->
	 [H | [X || X <- remove_dups(T), X /= H]].