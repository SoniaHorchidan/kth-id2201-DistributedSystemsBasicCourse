-module(rudy3).
-export([init/1, handler/1]).


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
			waitFromClient(Listen, Client, []);
		{error, Error} ->
			error 
	end.

waitFromClient(Listen, Client, UnresolvedRequest) ->
	Recv = gen_tcp:recv(Client, 0),

	case Recv of
		{ok, Str} ->
			NewRequest = UnresolvedRequest ++ Str,

			IsComplete = isRequestComplete(NewRequest),

			case IsComplete of
				true  ->
					resolve(Client, NewRequest),
					gen_tcp:close(Client),
					handler(Listen);
				false ->
					waitFromClient(Listen, Client, NewRequest)
			end;

		{error, Error} ->
			io:format("rudy: error ~w~n", [Error])
	end.

resolve(Client, Str) ->
	Request = http:parse_request(Str),
	Response = reply(Request),
	io:fwrite("~n~n" ++ Response, []), 		% removed to measure performance
	gen_tcp:send(Client, Response).


reply({{get, URI, _}, _, _}) ->
	http:ok("<html><head><title>Test</title></head><body>Some text<br/>" ++ URI ++ "</body></html>").


isRequestComplete(Request) ->
	hasHeader(Request) andalso hasBody(Request).


hasHeader(Request) ->
	Ans = string:str(Request, "\r\n\r\n"),
	if Ans > 0 ->
			true;
		true ->
			false
	end.

hasBody(Request) -> 
	{_, Headers, Body} = http:parse_request(Request),
	{HasProperty, ContentLength} = http:extractContentLength(Headers),
	case HasProperty of
		notfound ->
			true;
		found ->			
			BodyLength = string:len(Body),
			if ContentLength =:= BodyLength ->
				true;
			true ->
				false
			end
	end.


