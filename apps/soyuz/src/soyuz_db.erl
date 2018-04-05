-module(soyuz_db).
-include("../include/soyuz_fields.hrl").
-compile(export_all).

setup() ->
	mnesia:create_table(boards,
		[{attributes, record_info(fields, board)},
		 {record_name, board}
		]
	).

threads_name(Name) ->
	list_to_atom("threads_" ++ Name).

posts_name(Name) ->
	list_to_atom("posts_" ++ Name).

create_board(Name, Title, Header) ->
	Fun = fun() ->
		Board = #board {
			name = Name,
			title = Title,
			header = Header
		},
		mnesia:write(boards, Board, write)
	end,
	
	mnesia:create_table(
		threads_name(Name),
		[{attributes, record_info(fields, thread)},
		 {record_name, thread}
		]
	),
	mnesia:create_table(
		posts_name(Name),
		[{attributes, record_info(fields, post)},
		 {record_name, post}
		]
	),
	mnesia:transaction(Fun).

make_thread(Board, Subject, Post) ->
	Now = soyuz_util:unixtime(),
	Fun = fun() ->
		Thread = #thread {
			id = Now,
			subject = Subject,
			permasage = false,
			closed = false,
			bump_date = Now,
			post_count = 0
		},
		mnesia:write(threads_name(Board), Thread, write)
	end,
	mnesia:transaction(Fun),
	reply_time(Board, Now, Post, Now).
	
reply(Board, Thread_id, Post) ->
	reply_time(Board, Thread_id, Post, soyuz_util:unixtime()).

status(Board, Thread_id) ->
	Fun = fun() ->
		mnesia:read(threads_name(Board), Thread_id)
	end,
	case mnesia:transaction(Fun) of
		{atomic, []} ->
			no_such_thread;
		{atomic, [#thread{closed = true}]} ->
			thread_closed;
		{atomic, [_Thread]} ->
			ok;
		_ ->
			what_the_fuck
	end.

%% Incomplete:
%% checking for bump limit
%% 
reply_time(Board, Thread_id, {Name, Link, Body}, Time) ->
	Meat = fun() ->
		[Thread] = 
			mnesia:read(threads_name(Board), Thread_id),
		#thread{
			permasage = Permasage,
			bump_date = Bump_date,
			post_count = Post_count
		} = Thread,
		Post = #post {
			threadno_replyno = {Thread_id, Post_count + 1},
			date = Time,
			name = Name,
			link = Link,
			body = Body
		},
		mnesia:write(posts_name(Board), Post, write),
		New_thread = Thread#thread{
			bump_date = if
				Permasage -> Bump_date;
				true -> Time
			end,
			post_count = Post_count + 1
		},
		mnesia:write(threads_name(Board), New_thread, write)
	end,		
	Fun = fun() ->
		case status(Board, Thread_id) of
			ok ->
				Meat();
			Error ->
				Error
		end
	end,
	mnesia:transaction(Fun).