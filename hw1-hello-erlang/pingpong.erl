-module(pingpong).
-export([start/0, func/0]).

start() ->
	P = spawn(pingpong, func, []),
	P ! {self(), "Hello"},
	receive
		Msg -> io:format("(p1) Process responded with: ~s ~n", [Msg])
	end.


func() ->
	receive
		{From,Msg} -> io:format("(p2): Received a message from ~p : ~s ~n", [From, Msg]),
		From ! 'Got your message!'
	end.
