-module(soyuzweb_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) ->
	SupFlags = #{
		strategy => rest_for_one,
		intensity => 1,
		period => 5
	},
	ChildSpecs = [
	],
    {ok, {SupFlags, ChildSpecs}}.
