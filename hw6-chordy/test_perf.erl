-module(test_perf).
-export([test1/0, test2/0, test3/0, test4/0]).


test1() ->
	%% test one machine adding 10000 elements to one node vs 4 nodes
    Pid = node2:start(1),
    node2:start(250000000, Pid),
    node2:start(500000000, Pid),
    node2:start(750000000, Pid),
    run(Pid, 10000).


test2() ->
	%% test four machines adding 2500 elements each to one node vs 4 nodes
    First = node2:start(1),
    node2:start(250000000, First),
    node2:start(500000000, First),
    node2:start(750000000, First),
    spawn(fun() -> run(First, 2500) end),
    spawn(fun() -> run(First, 2500) end),
    spawn(fun() -> run(First, 2500) end),
    spawn(fun() -> run(First, 2500) end),
    timer:sleep(5000).


test3() ->
	%% test one machine adding 10000 elements to four nodes and then adding
	%% a new node to check if the new node takes over responsibility
    First = node2:start(1),
    node2:start(250000000, First),
    node2:start(500000000, First),
    node2:start(750000000, First),
    Elements = add(10000, First, []),

    io:fwrite("~n----------------------~n",[]),
    io:fwrite("Before adding a new node~n", []),
    timer:sleep(1000),
    First ! probe,

    timer:sleep(3000),

    io:fwrite("~n----------------------~n",[]),
    io:fwrite("After adding a new node~n", []),
    node2:start(350000000, First),
    timer:sleep(1000),
    First ! probe.



test4() ->
	%% test one machine adding 10000 elements to four nodes and then removing
	%% a node to check if the ring was maintained
    First = node3:start(1),
    node3:start(250000000, First),
    node3:start(500000000, First),
    Last = node3:start(750000000, First),

    io:fwrite("~n----------------------~n",[]),
    io:fwrite("Before removing a node~n", []),
    timer:sleep(1000),
    First ! probe,

    timer:sleep(3000),

    io:fwrite("~n----------------------~n",[]),
    io:fwrite("After removing the last node~n", []),
    Last ! stop,
    timer:sleep(1000),
    First ! probe.


run(Node, Num) ->    
    timer:sleep(1000),
    Elements = add(Num, Node, []),
    Time1 = now(),
    lookup(Node, Elements),
    Time2 = now(),
    io:format("Lookup time: ~w~n", [timer:now_diff(Time2, Time1)]).



add(0, Pid, Elements) ->
	Elements;
add(N, Pid, Elements) ->
	Key = key:generate(),
	Value = key:generate(),
	Qref = make_ref(),
	Pid ! {add, Key, Value, Qref, self()},
	receive
		{Qref, ok} ->
		    add(N - 1, Pid, Elements ++ [Key])
    end.


lookup(Pid, []) ->
	ok;
lookup(Pid, [Elem | Rest]) ->
	Qref = make_ref(),
    Pid ! {lookup, Elem, Qref, self()},
    receive
	{Qref, Result} ->
	    % io:format("~n Found key ~p: ~p~n", [Elem, Result]),
	    lookup(Pid, Rest)
    end.
	