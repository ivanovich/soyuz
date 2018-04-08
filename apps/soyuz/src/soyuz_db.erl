-module(soyuz_db).
-include("../include/soyuz_fields.hrl").
-include_lib("stdlib/include/qlc.hrl").
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
		mnesia:write(threads_name(Board), Thread, write),
		reply_time(Board, Now, Post, Now)
	end,
	mnesia:transaction(Fun).
	
reply(Board, Thread_id, Post) ->
	reply_time(Board, Thread_id, Post, soyuz_util:unixtime()).

%% Must be run within a transaction.
get_thread(Board, Thread_id) ->
	mnesia:read(threads_name(Board), Thread_id).

thread_status(Thread) ->
	case Thread of
		[] ->
			no_such_thread;
		[#thread{closed = true}] ->
			thread_closed;
		[_Thread] ->
			ok;
		_ ->
			what_the_fuck
	end.

%% Incomplete:
%% checking for bump limit
%% 
reply_time(Board, Thread_id, {Name, Link, Body}, Time) ->
	Meat = fun(Result) ->
		[Thread] = Result,
		#thread{
			permasage = Permasage,
			bump_date = Bump_date,
			post_count = Post_count
		} = Thread,
		Post = #post {
			threadno_replyno = {Thread_id, Post_count + 1},
			deleted = false,
			date = Time,
			name = Name,
			link = Link,
			body = Body
		},
		mnesia:write(posts_name(Board), Post, write),
		Updated_thread = Thread#thread{
			bump_date = if
				Permasage -> Bump_date;
				true -> Time
			end,
			post_count = Post_count + 1
		},
		mnesia:write(threads_name(Board), Updated_thread, write)
	end,		
	Fun = fun() ->
		Result = get_thread(Board, Thread_id),
		case thread_status(Result) of
			ok    -> Meat(Result);
			Error -> Error
		end
	end,
	mnesia:transaction(Fun).

is_in_thread(Post, Thread_id) ->
	{Threadno, _} = Post#post.threadno_replyno,
	Threadno == Thread_id.

view_thread(Board, Thread_id, Qualifier) ->
	Meat = fun(Result) ->
		[Thread] = Result,
		R = qlc:eval(qlc:q([P || 
			P <- mnesia:table(posts_name(Board)),
			is_in_thread(P, Thread_id)
		])),
		Rep = case Qualifier of
			{first, Many} ->
				lists:sublist(R, Many);
			{last, Many} ->
				[OP|S] = R,
				L = length(S),
				[OP|if
					Many < L -> lists:nthtail(L - Many, S);
					true     -> S
				end];
			all ->
				R
		end,
		[{thread, Thread}, {posts, Rep}]
	end,
	Fun = fun() ->
		Result = get_thread(Board, Thread_id),
		case thread_status(Result) of
			ok    -> Meat(Result);
			Error -> Error
		end
	end,
	mnesia:transaction(Fun).

thread_listing(Board, Qualifier) ->
	mnesia:transaction(fun() ->
		qlc:eval(qlc:q([T || T <- mnesia:table(threads_name(Board))]))
	end).

board_listing() ->
	mnesia:transaction(fun() ->
		qlc:eval(qlc:q([T || T <- mnesia:table(boards)]))
	end).

view_board(Board) ->
	mnesia:transaction(fun() ->
		[B] = mnesia:read(boards, Board), B
	end).