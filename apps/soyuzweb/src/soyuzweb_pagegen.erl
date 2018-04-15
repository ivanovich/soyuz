-module(soyuzweb_pagegen).

-export([build_all/0, build_board/1, build_mainpage/1]).

-include_lib("soyuz/include/soyuz_fields.hrl").

build_all() ->
	{atomic, Boards} = soyuz_db:board_listing(),
	Fun = fun(Board) ->
		#board{uri = URI} = Board,
		build_board(URI)
	end,
	lists:map(Fun, Boards).

build_board(URI) ->
	file:make_dir(filename:join(soyuzweb_util:doc_root() ++ [URI])),
	build_mainpage(URI).

build_mainpage(URI) ->
	L = soyuz_read:mainpage(soyuzweb, URI),
	{ok, File} = file:open(
		filename:join(soyuzweb_util:doc_root() ++ [URI] ++ ["index.html"]),
		write
	),
	{ok, Output} = soyuzweb_mainpage_tpl:render(L),
	io:fwrite(File, "~s", [Output]).

build_threadpage(URI, Threadno, Qualifier) ->
	L = soyuz_read:threadpage(soyuzweb, URI, Threadno, Qualifier),
	{ok, Output} = soyuzweb_threadpage_tpl:render(L).
%%	io:fwrite(File, "~s", [Output]).