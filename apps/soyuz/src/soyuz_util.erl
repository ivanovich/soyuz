-module(soyuz_util).

-export([unixtime/1]).

unixtime({MegaSecs, Secs, _MicroSecs}) ->
	MegaSecs * 1000000 + Secs.