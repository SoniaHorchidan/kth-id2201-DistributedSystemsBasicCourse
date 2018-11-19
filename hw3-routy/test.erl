-module(test).
-export([test_local/0, test_global/0, start_sweden/0, start_romania/0, start_norway/0, route_sweden/0, stop_sweden/0, stop_romania/0, stop_norway/0, add_links/0]).

start_sweden() ->
    routy:start(stockholm),
    routy:start(lund),
    routy:start(uppsala),
    routy:start(goteborg),
    routy:start(malmo),
    routy:start(linkoping),
    routy:start(norrkoping),
    routy:start(jonkoping),
    lund ! {add,stockholm,{stockholm,'sweden@127.0.0.1'}},
	stockholm ! {add,lund,{lund,'sweden@127.0.0.1'}},
	stockholm ! {add,uppsala,{uppsala,'sweden@127.0.0.1'}},
	uppsala ! {add,stockholm,{stockholm,'sweden@127.0.0.1'}},
	goteborg ! {add,lund,{lund,'sweden@127.0.0.1'}},
	lund ! {add,goteborg,{goteborg,'sweden@127.0.0.1'}},	
	malmo ! {add,lund,{lund,'sweden@127.0.0.1'}},
	lund ! {add,malmo,{malmo,'sweden@127.0.0.1'}},
	malmo ! {add,jonkoping,{jonkoping,'sweden@127.0.0.1'}},
	jonkoping ! {add,malmo,{malmo,'sweden@127.0.0.1'}},
	norrkoping ! {add,jonkoping,{jonkoping,'sweden@127.0.0.1'}},
	jonkoping ! {add,norrkoping,{norrkoping,'sweden@127.0.0.1'}},
	norrkoping ! {add,linkoping,{linkoping,'sweden@127.0.0.1'}},
	linkoping ! {add,norrkoping,{norrkoping,'sweden@127.0.0.1'}},
	stockholm ! {add,linkoping,{linkoping,'sweden@127.0.0.1'}},
	linkoping ! {add,stockholm,{stockholm,'sweden@127.0.0.1'}}.


route_sweden() ->
    Routers = [{lund, 'sweden@127.0.0.1'},{stockholm, 'sweden@127.0.0.1'},
    		  {uppsala, 'sweden@127.0.0.1'}, {malmo, 'sweden@127.0.0.1'},
    		  {linkoping, 'sweden@127.0.0.1'}, {norrkoping, 'sweden@127.0.0.1'},
    		  {jonkoping, 'sweden@127.0.0.1'}, {goteborg, 'sweden@127.0.0.1'}],
    lists:foreach(fun(Router) -> Router ! broadcast end, Routers),
    timer:sleep(1000),
    lists:foreach(fun(Router) -> Router ! update end, Routers),
    io:fwrite("~n-----------------~nSENDING MESSAGE FROM LUND TO UPPSALA ~n", []),
    lund ! {send, uppsala, "First message"},

    timer:sleep(1000),

    io:fwrite("~n-----------------~nSENDING MESSAGE FROM GOTEBORG TO UPPSALA ~n", []),
    goteborg ! {send, uppsala, "Second message"},

    timer:sleep(1000),

    io:fwrite("~n-----------------~nSENDING MESSAGE FROM GOTEBORG TO NORRKOPING ~n", []),
    goteborg ! {send, norrkoping, "Third message"},

    timer:sleep(1000),

    io:fwrite("~n-----------------~nSENDING MESSAGE FROM GOTEBORG TO MALMO ~n", []),
    goteborg ! {send, malmo, "Fourth message"},

    timer:sleep(1000),

    io:fwrite("~n-----------------~nSENDING MESSAGE FROM LUND TO JONKOPING ~n", []),
    lund ! {send, jonkoping, "Fifth message"},

    timer:sleep(1000),

    io:fwrite("~n-----------------~nSENDING MESSAGE FROM MALMO TO UPPSALA ~n", []),
    malmo ! {send, uppsala, "Sixth message"},

    timer:sleep(1000).

stop_sweden() ->
	routy:stop(stockholm),
	routy:stop(uppsala),
	routy:stop(lund),
	routy:stop(goteborg),
    routy:stop(malmo),
    routy:stop(linkoping),
    routy:stop(norrkoping),
    routy:stop(jonkoping).


test_local() ->
    start_sweden(),
    route_sweden(),
    stop_sweden().


start_romania() ->
    routy:start(bucharest),
    routy:start(iasi),
    routy:start(cluj),
    bucharest ! {add,iasi,{iasi,'romania@127.0.0.1'}},
    iasi ! {add,bucharest,{bucharest,'romania@127.0.0.1'}},
    bucharest ! {add,cluj,{cluj,'romania@127.0.0.1'}},
    cluj ! {add,bucharest,{bucharest,'romania@127.0.0.1'}}.


start_norway() ->
    routy:start(oslo).


add_links()->
    {iasi, 'romania@127.0.0.1'} ! {add,goteborg,{goteborg,'sweden@127.0.0.1'}},
    {goteborg, 'sweden@127.0.0.1'} ! {add,iasi,{iasi,'romania@127.0.0.1'}},
    {oslo, 'norway@127.0.0.1'} ! {add,cluj,{cluj,'romania@127.0.0.1'}},
    {cluj, 'romania@127.0.0.1'} ! {add,oslo,{oslo,'norway@127.0.0.1'}}.


stop_romania() ->
    routy:stop(cluj),
    routy:stop(iasi),
    routy:stop(bucharest).


stop_norway() ->
     routy:stop(oslo).


test_global() ->
    Routers = [{lund, 'sweden@127.0.0.1'},{stockholm, 'sweden@127.0.0.1'},
              {uppsala, 'sweden@127.0.0.1'}, {malmo, 'sweden@127.0.0.1'},
              {linkoping, 'sweden@127.0.0.1'}, {norrkoping, 'sweden@127.0.0.1'},
              {jonkoping, 'sweden@127.0.0.1'}, {goteborg, 'sweden@127.0.0.1'},
              {iasi,'romania@127.0.0.1'}, {cluj, 'romania@127.0.0.1'}, 
              {bucharest, 'romania@127.0.0.1'}, {norway, 'norway@127.0.0.1'}],
    lists:foreach(fun(Router) -> Router ! broadcast end, Routers),
    timer:sleep(1000),
    lists:foreach(fun(Router) -> Router ! update end, Routers),
    io:fwrite("~n-----------------~nSENDING MESSAGE FROM STOCKHOLM TO OSLO ~n", []),
    {stockholm, 'sweden@127.0.0.1'} ! {send, oslo, "First message"}.