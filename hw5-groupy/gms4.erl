-module(gms4).
-compile(export_all).
-define(timeout, 1000).
-define(arghh, 100).

leader(Id, Master, N, Slaves, Group, History) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, N, Msg}, Slaves),
			Master ! Msg,
			NewHistory = History ++ [{msg, N, Msg}],
			% io:fwrite("~nOLD: ~p   /// NEW: ~p~n", [length(History), length(NewHistory)]),
			leader(Id, Master, N + 1, Slaves, Group, NewHistory);
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			NewHistory = History ++ [{view, N, [self()|Slaves2], Group2}],
			leader(Id, Master, N + 1, Slaves2, Group2, NewHistory);
		{retrieve, From, FromMessage, ToMessage} ->
			%io:fwrite("~n FROM: ~p   TO: ~p   Hist: ~p~n", [FromMessage, ToMessage, History]),
			LostMessages = get_lost_messaged(FromMessage, ToMessage, History),
			From ! {retrieve, N, LostMessages};
		stop ->
			ok
	end.


slave(Id, Master, Leader, N, Last, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N , Last, Slaves, Group);
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, I, _} when I < N ->
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, I, Msg}->
			io:fwrite("~nEXPECTED: ~p /// GOT: ~p~n", [N, I]),
			Master ! Msg,
			slave(Id, Master, Leader, I + 1, {msg, N, Msg}, Slaves, Group);
		{msg, I, Msg} ->
			%io:fwrite("~nEXPECTED: ~p /// GOT: ~p~n", [N, I]),
			Leader ! {retrieve, self(), N + 1, I},
			slave(Id, Master, Leader, I + 1, {msg, I, Msg}, Slaves, Group);
		{view, N, [Leader|Slaves2], Group2} ->
			Master ! {view, Group2},
			slave(Id, Master, Leader, N + 1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, N, Last, Slaves, Group);
		{retrieve, I, LostMessagesList} ->
			%io:fwrite("~nList of lost messages: ~p~n", [LostMessagesList]),
			{ok, NewGroup, NewSlaves} = send_all_messages(Master, LostMessagesList, Group, Slaves),
			slave(Id, Master, Leader, I + 1, Last, NewSlaves, NewGroup);
		stop ->
			ok
	end.


send_all_messages(Master, [], Group, Slaves) ->
	{ok, Group, Slaves};
send_all_messages(Master, [{msg, _, Msg} | Tail], Group, Slaves) ->
	Master ! Msg,
	send_all_messages(Master, Tail, Group, Slaves);
send_all_messages(Master, [{view, _, [Leader|Slaves2], Group2} | Tail], Group, Slaves) ->
	Master ! {view, Group2},
	send_all_messages(Master, Tail, Group2, Slaves2);
send_all_messages(Master, [_ | Tail], Group, Slaves) ->
	send_all_messages(Master, Tail, Group, Slaves).


get_lost_messaged(To, To, History) ->
	[lists:keyfind(To, 2, History)];
get_lost_messaged(From, To, History) ->
	[lists:keyfind(From, 2, History)] ++ get_lost_messaged(From + 1, To, History).


% bcast(Id, Msg, Peers) ->
% 	[Peer ! Msg || Peer <- Peers].


bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).


crash(Id) ->
	case random:uniform(?arghh) of
		?arghh ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.


election(Id, Master, N, Last, Slaves, [_|Group]) ->
	Self = self(),
	case Slaves of
		[Self|Rest] ->
			bcast(Id, Last, Rest),
			bcast(Id, {view, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master, N + 1, Rest, Group, []);
		[Leader|Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N , Last, Rest, Group)
	end.


start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.


init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 0, [], [Master], []).


start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.


init(Id, Rnd, Grp, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, N, [Leader|Slaves], Group} ->
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N + 1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
	after ?timeout ->
		Master ! {error, "no reply from leader"}
	end.