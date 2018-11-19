-module(testChunked).
-export([test/0, test1/3]).


test() ->
	Host = "127.0.0.1",
	Port = 8080,
	Opt = [list, {active, false}, {reuseaddr, true}],
	io:fwrite("~nFirst test: ", []),
	test1(Host, Port, Opt),
	io:fwrite("~nSecond test: ", []),
	test2(Host, Port, Opt),
	ok.
	

test1(Host, Port, Opt) ->
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, "GET " ++ "/foo" ++ " HTTP/1.1\r\nfoo 34\r\n"),
	gen_tcp:send(Server, "\r\n" ++ "<html><head><title>Test1</title></head><body>Some text<br/>"),

	Recv = gen_tcp:recv(Server, 0),

	case Recv of
		{ok, _} ->
			io:fwrite("ok ~n", []);
		{error, Error} ->
			io:format("test: error: ~w~n", [Error])
	end,

	gen_tcp:close(Server).


test2(Host, Port, Opt) ->
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, "GET " ++ "/foo" ++ " HTTP/1.1\r\nContent-Length: 12\r\n"),
	gen_tcp:send(Server, "\r\n" ++ "Hello "),
	gen_tcp:send(Server, "world!"),


	Recv = gen_tcp:recv(Server, 0),

	case Recv of
		{ok, _} ->
			io:fwrite("ok ~n", []);
		{error, Error} ->
			io:format("test: error: ~w~n", [Error])
	end,
	gen_tcp:close(Server).
