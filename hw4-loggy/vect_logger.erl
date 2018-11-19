-module(vect_logger).
-export([start/1, stop/1]).


start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).


stop(Logger) ->
	Logger ! stop.


init(Nodes) ->
	loop(vect:clock(Nodes), [], 0).


loop(Clock, HoldBackQueue, MaxQueueLength) ->
	receive
		{log, From, Time, Msg} ->
			NewClock = vect:update(From, Time, Clock),
			NewHoldBackQueue = HoldBackQueue ++ [{From, Time, Msg}],
			SortedHoldBackQueue = lists:sort(fun vect:sortMessages/2, NewHoldBackQueue),
			NewQueue = checkQueue(SortedHoldBackQueue, NewClock),
			MaxLength = max(MaxQueueLength, length(NewQueue)),
			loop(NewClock, NewQueue, MaxLength);
		stop ->
			io:format("~nprinting messaged left in queue.....~n", []),
			lists:foreach(fun({From, Time, Msg}) -> log(From, Time, Msg) end, HoldBackQueue),
			timer:sleep(1000),
			io:format("~nMaximum queue length: ~p ~n", [MaxQueueLength]),
			ok 
	end.


log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).


checkQueue(Queue, Clock) ->
	checkQueue(Queue, Clock, []).


checkQueue([], _, Queue) ->
	Queue;
checkQueue([{From, Time, Msg} |  Tail], Clock, Queue) ->
	case vect:safe(Time, Clock) of
		true ->
			log(From, Time, Msg),
			checkQueue(Tail, Clock, Queue);
		false ->
			NewQueue = lists:sort(fun vect:sortMessages/2, Queue ++ [{From, Time, Msg}]),
			checkQueue(Tail, Clock, NewQueue)
	end.

