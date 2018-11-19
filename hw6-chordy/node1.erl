-module(node1).
-compile(export_all).
-define(Stabilize, 100).
-define(Timeout, 1000).

node(Id, Predecessor, Successor) ->
	receive
		{key, Qref, Peer} ->
			%% a peer needs to know our key
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);

		{notify, New} ->
			%% a new node informs us of its existence
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);

		{request, Peer} ->
			%% a predecessor needs to know our predecessor
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);

		{status, Pred} ->
			%% our successor informs us about its predecessor
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);

		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);

		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor);

		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor);

		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor)
	end.


stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			%% pointing to nothing
			Spid ! {notify, {Id, self()}},
			Successor;

		{Id, _} ->
			%%% pointing to us
			Successor;

		{Skey, _} ->
			%% pointing to itself
			Spid ! {notify, {Id, self()}},
			Successor;

		{Xkey, Xpid} ->
			%% pointing to some other node
			case key:between(Xkey, Id, Skey) of
				true ->
					Xpid ! {request, self()},
					{Xkey, Xpid};
				false ->
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.


stabilize({_, Spid}) ->
	Spid ! {request, self()}.


notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, Npid};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					{Nkey, Npid};
				false ->
					Predecessor
			end
	end.


schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).



request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
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
	node(Id, Predecessor, Successor).


connect(Id, nil) ->
	{ok, {Id, self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey, Peer}}
	after ?Timeout ->
		io:format("Time out: no response~n",[])
	end.


create_probe(Id, {Skey, Spid}) ->
	Time = erlang:system_time(micro_seconds),
	Spid ! {probe, Id, [Id], Time},
	io:fwrite("~n~p created probe and forwarded to ~p~n", [Id, Skey]).


remove_probe(T, Nodes) ->
	NewTime = erlang:system_time(micro_seconds),
	TimePassed = NewTime - T,
	io:fwrite("~nprobe ended: ~p ~n", [TimePassed]).


forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
	Spid ! {probe, Ref, Nodes ++ [Id], T},
	io:fwrite("~n~p forwarded probe to ~p~n", [Id, Skey]).