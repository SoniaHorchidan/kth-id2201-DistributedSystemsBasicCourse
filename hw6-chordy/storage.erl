-module(storage).
-compile(export_all).

create() ->
	[].


add(Key, Value, Store) ->
	Store ++ [{Key, Value}].


lookup(Key, Store) ->
	lists:keyfind(Key, 1, Store).


split(From, To, Store) ->
	lists:partition(fun({Key, _}) -> (Key > From) and (Key =< To)  end, Store).
	

merge(Entries, Store) ->
	lists:merge(Entries, Store).


