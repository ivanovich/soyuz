-module(soyuzweb_pagegen).

-export([build_all/0, build/1]).

build_all() ->
	{atomic, Boards} = soyuz_db:board_listing(),
	Fun = fun(Board) ->
		{Name, _Title, _Desc} = Board,
		build(Name)
	end,
	lists:map(Fun, Boards).

build(Board) ->
	soyuz_read:mainpage(Board).