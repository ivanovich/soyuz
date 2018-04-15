%% Read-only access to boards, for rendering and whatnot. Technically
%% the ``view'' of MVC, but I refuse to legitimise that convention.

-module(soyuz_read).

-export([mainpage/2, threadpage/4, backlogpage/2]).

-include_lib("soyuz/include/soyuz_fields.hrl").

mainpage(App, BoardURI) ->
	Conf = soyuz_config:combined_config(BoardURI, App),
	{atomic, [Board]} = soyuz_db:get_board(BoardURI),
	{atomic, Threads} = soyuz_db:list_board(BoardURI),
	Fun = fun(Thread) ->
		{atomic, S} = soyuz_db:read_thread(
			BoardURI,
			Thread#thread.threadno,
			{last, proplists:get_value(replies_per_thread, Conf)}
		),
		S
	end,
	[
		{board, Board},
		{index, lists:sublist(
			Threads,
			proplists:get_value(threads_listed, Conf)
		)},
		{threads, lists:map(Fun, lists:sublist(
			Threads,
			proplists:get_value(threads_displayed, Conf)
		))}
	].

threadpage(App, BoardURI, Threadno, Qualifier) ->
	{atomic, [Board]} = soyuz_db:get_board(BoardURI),
	{atomic, Thread} = soyuz_db:read_thread(BoardURI, Threadno, Qualifier),
	Conf = soyuz_config:combined_config(BoardURI, App),
	PageConf = soyuz_config:page_forward(Conf, App),
	[{board, Board}, {config, PageConf}] ++ [Thread].

backlogpage(App, BoardURI) ->
	{atomic, [Board]} = soyuz_db:get_board(BoardURI),
	{atomic, Threads} = soyuz_db:list_board(BoardURI),
	Conf = soyuz_config:combined_config(BoardURI, App),
	PageConf = soyuz_config:page_forward(Conf, App),
	[
		{board, Board},
		{config, PageConf},
		{threads, Threads}
	].