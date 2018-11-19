-module(testParallel).
-export([test/0]).


test() ->
	Self = self(),
    Pids = [ spawn_link(fun() -> Self ! {self(), {X, test:bench("127.0.0.1", 8080)}} end) || X <- lists:seq(1, 3) ],

    [ receive {Pid, R} -> R end || Pid <- Pids ].