-module(rudy2).
-export([init/1, handler/1, request/1]).


init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			handler(Listen),
			gen_tcp:close(Listen), 
			ok;
		{error, Error} ->
			error 
	end.


handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			ok = request(Client),
			%% ok = gen_tcp:close(Client),
			handler(Listen);
		{error, Error} ->
			error 
	end.


request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		{ok, Str} ->
			request(Client, Str);
		{error, Error} ->
			io:format("rudy: error ~w~n", [Error])
	end.

request(Client, PartialRequest) ->
	IsComplete = isRequestComplete(PartialRequest),
	io:fwrite("~n~n" ++ PartialRequest ++ "~n~n", []),
	case IsComplete of
		true ->
			io:fwrite("\nComplete\n"),
			Request = http:parse_request(PartialRequest),
			Response = reply(Request),
			io:fwrite(Response, []), 		% removed to measure performance
			gen_tcp:send(Client, Response),
			gen_tcp:close(Client);
		false ->
			io:fwrite("\nNot Complete\n"),
			Recv = gen_tcp:recv(Client, 0),
			case Recv of
				{ok, Str} ->
					request(Client, PartialRequest ++ Str);
				{error, Error} ->				
					io:format("rudy: error ~w~n", [Error])
			end
	end.

isRequestComplete(Request) ->
	hasHeader(Request). %%  andalso hasBody(Request).

hasHeader(Request) ->
	Ans = string:str("\r\n\r\n", Request),
	io:fwrite(Request, []),
	io:fwrite("~p", [Ans]),
	if Ans > 0 ->
			true;
		true ->
			false
	end.


reply({{get, URI, _}, _, _}) ->
	timer:sleep(40),
	http:ok("<html><head><title>Test1</title></head><body>Some text<br/>" ++ URI ++ "</body></html>").

