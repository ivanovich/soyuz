%% Read-only access to boards, for rendering and whatnot. Technically
%% the ``view'' of MVC, but I refuse to legitimise that convention.

-module(soyuz_read).

-export([mainpage/1, threadpage/3, backlogpage/1]).

-include_lib("soyuz/include/soyuz_fields.hrl").

mainpage(BoardURI) ->
	{atomic, [Board]} = soyuz_db:get_board(BoardURI),
	{atomic, Threads} = soyuz_db:list_board(BoardURI),
	Fun = fun(Thread) ->
		{atomic, S} = soyuz_db:read_thread(
			BoardURI,
			Thread#thread.threadno,
			{last, 10}
		),
		S
	end,
	[
		{board, Board},
		{index, lists:sublist(Threads, 40)},
		{threads, lists:map(Fun, lists:sublist(Threads, 10))}
	].

threadpage(BoardURI, Threadno, Qualifier) ->
	{atomic, Board} = soyuz_db:get_board(BoardURI),
	{atomic, Thread} = soyuz_db:read_thread(BoardURI, Threadno, Qualifier),
	[{board, Board}|Thread].

backlogpage(BoardURI) ->
	{atomic, Board} = soyuz_db:get_board(BoardURI),
	{atomic, Threads} = soyuz_db:list_board(BoardURI),
	[
		{board, Board},
		{threads, Threads}
	].