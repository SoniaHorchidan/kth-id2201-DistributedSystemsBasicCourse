-module(node3).
-compile(export_all).
-define(Stabilize, 10).
-define(Timeout, 10000).

node(Id, Predecessor, Successor, Store, Next) ->
	receive
		{key, Qref, Peer} ->
			%% a peer needs to know our key
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store, Next);

		{notify, New} ->
			%% a new node informs us of its existence
			{Pred, NewStore} = notify(New, Id, Predecessor, Store),
			node(Id, Pred, Successor, NewStore, Next);

		{request, Peer} ->
			%% a predecessor needs to know our predecessor
			request(Peer, Predecessor, Successor),
			node(Id, Predecessor, Successor, Store, Next);

		{status, Pred, Nx} ->
			%% our successor informs us about its predecessor
			{Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
			node(Id, Predecessor, Succ, Store, Nxt);

		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Store, Next);

		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor, Store, Next);

		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor, Store, Next);

		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor, Store, Next);

		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added, Next);
			
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store, Next);

		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Merged, Next);

		{'DOWN', Ref, process, _, _} ->
			{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
			node(Id, Pred, Succ, Store, Nxt);

		status ->
			io:fwrite("~nPred: ~p ~nSucc: ~p; ~nNext: ~p~n", [Predecessor, Successor, Next]),
			node(Id, Predecessor, Successor, Store, Next);

		stop ->
			ok
	end.


stabilize(Pred, Nx, Id, Successor) ->
	{Skey, Sref, Spid} = Successor,
	case Pred of
		nil ->
			%% pointing to nothing
			Spid ! {notify, {Id, self()}},
			{Successor, Nx};

		{Id, _} ->
			%%% pointing to us
			{Successor, Nx};

		{Skey, _} ->
			%% pointing to itself
			Spid ! {notify, {Id, self()}},
			{Successor, Nx};

		{Xkey, Xpid} ->
			%% pointing to some other node
			case key:between(Xkey, Id, Skey) of
				true ->
					Xpid ! {request, self()},
					Xref = monitor(Xpid),
					drop(Sref),
					{{Xkey, Xref, Xpid}, Successor};
				false ->
					Spid ! {notify, {Id, self()}},
					{Successor, Nx}
			end
	end.


stabilize({_, _,  Spid}) ->
	Spid ! {request, self()}.



notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Id, Store, Nkey, Npid),
			Nref = monitor(Npid),
			{{Nkey, Nref, Npid}, Keep};
		{Pkey, Pref, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Id, Store, Nkey, Npid),
					Nref = monitor(Npid),
					drop(Pref),
					{{Nkey, Nref, Npid}, Keep};
				false ->
					{Predecessor, Store}
			end
	end.


handover(Id, Store, Nkey, Npid) ->
	{Keep, Rest} = storage:split(Nkey, Id, Store),
	Npid ! {handover, Rest},
	Keep.


schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).


request(Peer, Predecessor, Next) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil, Next};
		{Pkey, _, Ppid} ->
			Peer ! {status, {Pkey, Ppid}, Next}
	end.


add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of 
		true ->
			Client ! {Qref, ok},
			storage:add(Key, Value, Store);
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.


lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
	case key:between(Key, Pkey, Id) of 
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {lookup, Key, Qref, Client}
	end.



start(Id) ->
	start(Id, nil).


start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).


init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor, storage:create(), nil).


connect(Id, nil) ->
	{ok, {Id, nil, self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			Sref = monitor(Peer),
			{ok, {Skey, Sref, Peer}}
	after ?Timeout ->
		io:format("Time out: no response~n",[])
	end.


create_probe(Id, {Skey, _, Spid}) ->
	Time = erlang:system_time(micro_seconds),
	Spid ! {probe, Id, [Id], Time},
	io:fwrite("~n~p created probe and forwarded to ~p~n", [Id, Skey]).


remove_probe(T, Nodes) ->
	NewTime = erlang:system_time(micro_seconds),
	TimePassed = NewTime - T,
	io:fwrite("~nprobe ended: ~p ~n", [TimePassed]).


forward_probe(Ref, T, Nodes, Id, {Skey, _, Spid}) ->
	Spid ! {probe, Ref, Nodes ++ [Id], T},
	io:fwrite("~n~p forwarded probe to ~p~n", [Id, Skey]).


monitor(Pid) ->
	erlang:monitor(process, Pid).


drop(nil) ->
	ok;
drop(Pid) ->
	erlang:demonitor(Pid, [flush]).


down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, OldRef, Npid}) ->
	drop(OldRef),
    Nref = monitor(Npid),
    {Predecessor, {Nkey, Nref, Npid}, nil}.