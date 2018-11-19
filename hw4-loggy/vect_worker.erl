-module(vect_worker).
-export([start/5, stop/1, peers/2]).


start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).


stop(Worker) ->
	Worker ! stop.


init(Name, Log, Seed, Sleep, Jitter) ->
	random:seed(Seed, Seed, Seed),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, vect:zero());
		stop ->
			ok
	end.


peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.


loop(Name, Log, Peers, Sleep, Jitter, TimeStamp)->
	Wait = random:uniform(Sleep),
	receive
		{msg, Time, Msg} ->
			NewTimeStamp = vect:inc(Name, vect:merge(TimeStamp, Time)),
			Log ! {log, Name, NewTimeStamp, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, NewTimeStamp);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
	after Wait ->
		Selected = select(Peers),
		NewTimeStamp = vect:inc(Name, TimeStamp),
		Message = {hello, random:uniform(100)},
		Selected ! {msg, NewTimeStamp, Message},
		jitter(Jitter),
		Log ! {log, Name, NewTimeStamp, {sending, Message}},
		loop(Name, Log, Peers, Sleep, Jitter, NewTimeStamp)
	end.


select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers).


jitter(0) -> ok;
	jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).