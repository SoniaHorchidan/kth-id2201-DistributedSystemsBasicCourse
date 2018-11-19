-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() -> 
	[].


add(Name, Ref, Pid, Intf) ->
	case lists:member({Name, Ref, Pid}, Intf) of 
		true ->
			Intf;
		false ->
			Intf ++ [{Name, Ref, Pid}]
	end.


remove(Name, Intf) ->
	Found = lists:keyfind(Name, 1, Intf),
	case Found of
		false -> 
			ok;
		_ ->
			lists:keydelete(Name, 1, Intf)
	end.


lookup(Name, Intf) ->
	Found = lists:keyfind(Name, 1, Intf),
	case Found of
		false -> 
			notfound;
		_ ->
			{_, _, Pid} = Found,
			{ok, Pid}
	end.


ref(Name, Intf) ->
	Found = lists:keyfind(Name, 1, Intf),
	case Found of
		false -> 
			notfound;
		_ ->
			{_, Ref, _} = Found,
			{ok, Ref}
	end.


name(Ref, Intf) ->
	Found = lists:keyfind(Ref, 2, Intf),
	case Found of
		false ->
			notfound;
		_ -> 
			{Name, _, _} = Found,
			{ok, Name}
	end.


list(Intf) ->
	[Name || {Name, _, _} <- Intf].


broadcast(Message,Intf) ->
    lists:foreach(fun({_,_,Pid})->
                          Pid ! Message
                  end, Intf).