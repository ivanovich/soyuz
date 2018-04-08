%% Read-only access to boards, for rendering and whatnot. Technically
%% the ``view'' of MVC, but I refuse to legitimise that convention.

-module(soyuz_read).

-export([mainpage/1]).

-include_lib("soyuz/include/soyuz_fields.hrl").

mainpage(Board_id) ->
	{atomic, Threads} = soyuz_db:thread_listing(Board_id, all),
	{atomic, Board} = soyuz_db:view_board(Board_id),
	Fun = fun(Thread) ->
		{atomic, S} = soyuz_db:view_thread(
			Board_id,
			Thread#thread.id,
			{last, 10}
		),
		S
	end,
	[
		{board, Board},
		{index, lists:sublist(Threads, 40)},
		{threads, lists:map(Fun, lists:sublist(Threads, 10))}
	].