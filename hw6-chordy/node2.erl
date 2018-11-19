-module(node2).
-compile(export_all).
-define(Stabilize, 100).
-define(Timeout, 1000).

node(Id, Predecessor, Successor, Store) ->
	receive
		{key, Qref, Peer} ->
			%% a peer needs to know our key
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store);

		{notify, New} ->
			%% a new node informs us of its existence
			{Pred, NewStore} = notify(New, Id, Predecessor, Store),
			node(Id, Pred, Successor, NewStore);

		{request, Peer} ->
			%% a predecessor needs to know our predecessor
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor, Store);

		{status, Pred} ->
			%% our successor informs us about its predecessor
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ, Store);

		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Store);

		probe ->
			create_probe(Id, Successor),
			io:fwrite("~nNode ~p has ~p elements in store", [Id, length(Store)]),
			node(Id, Predecessor, Successor, Store);

		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor, Store);

		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			io:fwrite("~nNode ~p has ~p elements in store", [Id, length(Store)]),
			node(Id, Predecessor, Successor, Store);

		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added);
			
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store);

		{handover, Elements} ->
			Merged = storage:merge(Elements, Store),
			node(Id, Predecessor, Successor, Merged)

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


notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Id, Store, Nkey, Npid),
			{{Nkey, Npid}, Keep};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Id, Store, Nkey, Npid),
					{{Nkey, Npid}, Keep};
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



request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.


add(Key, Value, Qref, Client, Id, nil, {_, Spid}, Store) ->
	Spid ! {add, Key, Value, Qref, Client},
	Store;
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of 
		true ->
			Client ! {Qref, ok},
			storage:add(Key, Value, Store);
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

% lookup(Key, Qref, Client, Id, nil, Successor, Store) ->
% 	Result = storage:lookup(Key, Store),
% 	Client ! {Qref, Result};
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
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
	node(Id, Predecessor, Successor, storage:create()).


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
	Spid ! {probe, Id, [Id], Time}.
	%io:fwrite("~n~p created probe and forwarded to ~p~n", [Id, Skey]).


remove_probe(T, Nodes) ->
	NewTime = erlang:system_time(micro_seconds).
	%io:fwrite("~nprobe ended: ~p microseconds ~n", [NewTime - T]).


forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
	Spid ! {probe, Ref, Nodes ++ [Id], T}.
	%io:fwrite("~n~p forwarded probe to ~p~n", [Id, Skey]).