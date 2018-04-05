-module(soyuz_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(soyuz_sup, []).

init(_Args) ->
	SupFlags = #{
		strategy => rest_for_one,
		intensity => 1,
		period => 5
	},
	ChildSpecs = [
		#{
			id => soyuz_chassis,
			start => {soyuz_chassis, start_link, []},
			restart => permanent,
			shutdown => brutal_kill,
			type => worker,
			modules => [soyuz_chassis]
		}
	],
	{ok, {SupFlags, ChildSpecs}}.
	