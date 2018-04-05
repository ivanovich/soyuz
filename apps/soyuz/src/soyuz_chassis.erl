-module(soyuz_chassis).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
	gen_server:start_link(
		{local, soyuz_chassis},
		soyuz_chassis,
		[],
		[]).

init(_Args) ->
	{ok, whatever}.

handle_call(_Request, _From, Chs) ->
	{reply, replied, Chs}.

handle_cast(_Request, Chs) ->
	{noreply, Chs}.
