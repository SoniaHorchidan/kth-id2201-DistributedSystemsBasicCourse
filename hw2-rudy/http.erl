-module(http).
-export([parse_request/1, ok/1, get/1, extractContentLength/1]).

% Request = Request-Line
% 	*(( general-header
% 		| request-header
% 		| entity-header ) CRLF)
% 	CRLF
% 	[ message-body ]


parse_request(R0) ->
	{Request, R1} = request_line(R0),
	{Headers, R2} = headers(R1),
	{Body, _} = message_body(R2),			
	{Request, Headers, Body}.


%% Request-Line = Method SP Request-URI SP HTTP-Version CRLF 
%% --- where SP = space
%% 32 - ASCII value for space
request_line([$G, $E, $T, 32 | R0]) ->
	{URI, R1} = request_uri(R0),
	{Ver, R2} = http_version(R1),
	[13, 10 | R3] = R2,
	{{get, URI, Ver}, R3}.


request_uri([32 | R0]) ->
	{[], R0};
request_uri([C | R0]) ->
	{Rest, R1} = request_uri(R0),
	{[C | Rest], R1}.


http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
	{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
	{v10, R0}.


%% headers
headers([13, 10 | R0]) ->
	{[], R0};
headers(R0) ->
	{Header, R1} = header(R0),
	{Rest, R2} = headers(R1),
	{[Header | Rest], R2}.

header([13, 10 | R0]) ->
	{[], R0};
header([C | R0]) ->
	{Rest, R1} = header(R0),
	{[C | Rest], R1}.


%% body
message_body(R) ->
	{R, []}.


%% replies
ok(Body) -> 
	"HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
	"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".


extractContentLength([]) ->
	{notfound, 0};
extractContentLength([[$C, $o, $n, $t, $e, $n, $t, $-, $L, $e, $n, $g, $t, $h, 58, 32 | Num] | _]) ->
	%% 58 - ASCII code for ":"
	%% 32 - ASCII code for " "
	{found, list_to_integer(Num)};
extractContentLength([_ | Tail]) ->
	extractContentLength(Tail).

