-module(soyuz_db).

%% This module nicely (and relatively efficiently) wraps up all useful
%% basic database operations, so other modules do not have to make
%% ad-hoc calls to mnesia. This is not a user interface. `soyuz_read`
%% and `soyuz_post` exist for that. All this module does beyond wrapping is
%% adjusting the appropriate metadata such as bumps and other dates.
%% Tripcode processing and the like is done by the aforementioned modules
%% that use this one.

-export([setup/0]).
-export([create_board/3, get_board/1, delete_board/1]).
-export([create_thread/3, get_thread/2, read_thread/3, delete_thread/2]).
-export([is_in_thread/2, thread_status/1]).
-export([create_post/3, read_post/3, delete_post/3]).

-include_lib("soyuz/include/soyuz_fields.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% Exports.

setup() ->
	mnesia:create_table(
		boards,
		[{attributes, record_info(fields, board)}, {record_name, board}]
	).

create_board(URI, Title, Header) ->
	Fun = fun() ->
		case mnesia:read(boards, URI) of
			[] ->
				Board = #board {
					uri = URI,
					title = Title,
					header = Header
				},
				mnesia:write({boards, Board});
			_ ->
				{board_already_exists, URI}
		end
	end,
	case mnesia:transaction(Fun) of
		{atomic, {board_already_exists, URI}} ->
			{board_already_exists, URI}
		{atomic, _} ->
			mnesia:create_table(
				threads_name(URI),
				[{attributes, record_info(fields, thread)},
				 {record_name, thread}
				]
			),
			mnesia:create_table(
				posts_name(URI),
				[{attributes, record_info(fields, post)},
				 {record_name, post}
				]
			)
	end.
		

%% Returns the info of the board, if it exists, pointed to by the URI.
%% This is simply the entry in the boards table; this does not give you
%% the threads or posts.
get_board(URI) ->
	mnesia:transaction(fun() ->
		mnesia:read(boards, URI)
	end).

%% This drops the board and its corresponding threads and posts table.
%% Use with caution!
delete_board(URI) ->
	mnesia:transaction(fun() ->
		mnesia:delete({boards, URI}),
		mnesia:delete_table(threads_name(URI)),
		mnesia:delete_table(posts_name(URI))
	end).

%% For information regarding the treatment of `Post`, see `create_post/3`.
%% In the unlikely event that two threads are being created within the
%% same second, this routine, within a mnesia transaction, checks for a
%% thread number collision and increments the date by one second.
create_thread(BoardURI, Subject, Post) ->
	Meat = fun() ->
		Now = os:timestamp(),
		Unix = soyuz_util:unixtime(Now),
		Thread = #thread {
			threadno = Unix,
			subject = Subject,
			permasage = false,
			closed = false,
			bump_date = Now,
			post_count = 0
		},
		mnesia:write(threads_name(BoardURI), Thread, write),
		create_post_unchecked(BoardURI, Thread#thread.threadno, Post#post{
			date = Now
		})
	end,
	transaction_board_protected(Meat).

%%
get_thread(BoardURI, Threadno) ->
	Meat = fun() ->
		mnesia:read(threads_name(BoardURI), Threadno)
	end,
	transaction_board_protected(Meat).

%% This 
read_thread(BoardURI, Threadno, Qualifier) ->
	InnerMeat = fun(Result) ->
		[Thread] = Result,
		R = qlc:eval(qlc:q([P ||
			P <- mnesia:table(posts_name(BoardURI)),
			is_in_thread(P, Threadno)
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
	transaction_thread_protected(InnerMeat).

delete_thread(BoardURI, Threadno) ->
	InnerMeat = fun(Result) ->
		mnesia:delete({threads_name(BoardURI), Result#thread.threadno}),
		qlc:eval(qlc:q([
			mnesia:delete({posts_name(BoardURI), P#post.threadno_replyno})
			|| P <- mnesia:table(posts_name(BoardURI)),
			   is_in_thread(P, Threadno)
		]))
	end,
	transaction_thread_protected(InnerMeat).

is_in_thread(Post, Threadno) ->
	{Threadno_, _} = Post#post.threadno_replyno,
	Threadno_ == Threadno.

thread_status(Result) ->
	case Result of
		[] ->
			no_such_thread;
		[#thread{closed = true}] ->
			thread_closed;
		[_Thread] ->
			ok;
		_ ->
			what_the_fuck
	end.

%% `Post` is a skeletal instance of the `post` record, with `date`
%% set if it is `undefined`. The one instance in which it is not
%% `undefined` upon reaching this function is thread creation, so that
%% the timestamp can exactly match the UNIX epoch time in the thread number.
create_post(BoardURI, Threadno, Post) ->
	InnerMeat = fun(Result) ->
		create_post_unchecked(BoardURI, Result, Post);
	end,
	transaction_thread_protected(InnerMeat).


read_post(BoardURI, Threadno, Replyno) ->
	ok.

delete_post(BoardURI, Threadno, Replyno) ->
	ok.


%% Utility functions. Yes, these generate atoms, which is unambiguously a
%% Bad Thing, but this is the only place in the system this ever happens,
%% and with the exception of board creation, synthesis of new atoms by
%% these functions is never exposed via any user interface. The limited
%% number of atoms that will ever be created in ordinary usage is far smaller
%% than any common limit. Atom leak is estimated at tens or a few hundred
%% over years of operation.
%% All functions that call threads_name/1 or posts_name/1, with the
%% exception of create_board/3, check for the existence of the board in
%% the boards table before generating any atoms. If the subsequent atom
%% would denote a non-existent table, this is stopped before atom leak
%% can occur. This fills this module with some very un-Erlang-like defensive
%% code regarding these atoms, but effort has been made to ensure that
%% the code is no more defensive than absolutely necessary to prevent
%% atom leak.

threads_name(Name) ->
	list_to_atom("threads_" ++ Name).

posts_name(Name) ->
	list_to_atom("posts_" ++ Name).

%% Internal use only, to avoid nesting. Must be run within a transaction.
create_post_unchecked(BoardURI, Thread, Post) ->
	Date = os:timestamp(),
	NewPost = Post#post {
		threadno_replyno = {
			Thread#thread.threadno,
			Thread#thread.post_count + 1
		},
		date = Date
	},
	NewThread = Thread#thread{
		bump_date = if
			Thread#thread.permasage -> Thread#thread.bump_date;
			true                    -> Post#post.date
		end,
		post_count = Thread#thread.post_count + 1
	},
	mnesia:write({posts_name(BoardURI), NewPost}),
	mnesia:write({threads_name(BoardURI), NewThread}).

transaction_thread_protected(InnerMeat) ->
	Meat = fun() ->
		Result = mnesia:read(threads_name(BoardURI), Threadno),
		case thread_status(Result) of
			ok    -> InnerMeat(Result);
			Error -> Error
		end
	end,
	transaction_board_protected(Meat).

transaction_board_protected(Meat) ->
	Fun = fun() ->
		case mnesia:read(Boards, BoardURI) of
			[]      -> {no_such_board, BoardURI};
			[Board] -> Meat()
		end
	end,
	mnesia:transaction(Fun).