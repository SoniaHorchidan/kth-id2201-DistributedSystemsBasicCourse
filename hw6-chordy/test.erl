-module(test).
-compile(export_all).


test1(N) ->
	%% start the first node
	First = node1:start(1),

	%% start N nodes and adding them to the ring
	[node1:start(Id, First) || Id <- lists:seq(2, N)],

	%% run a probe around the ring
	timer:sleep(1000),
	First ! probe.


test2(N) ->
	%% start the first node
	First = node2:start(1),

	%% start N nodes and adding them to the ring
	[node2:start(Id, First) || Id <- lists:seq(1, N)],

	%% run a probe around the ring
	timer:sleep(1000),
	First ! probe.

	