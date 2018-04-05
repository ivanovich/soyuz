-module(soyuz_util).

-export([unixtime/0]).

unixtime() ->
	fun({MegaSecs, Secs, _MicroSecs}) ->
		MegaSecs * 1000000 + Secs
	end(os:timestamp()).