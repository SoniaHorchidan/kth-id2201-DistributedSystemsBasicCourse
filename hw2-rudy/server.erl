-module(server).
-export([start/1, stop/0]).


start(Port) ->
	register(rudy, spawn(fun() -> rudy3:init(Port) end)).

stop() -> 
	exit(whereis(rudy), "time to die").

