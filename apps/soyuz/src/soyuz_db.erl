-module(soyuz_db).

-export([setup/0]).
-export([create_board/3, get_board/1, list_board/1, delete_board/1]).
-export([create_thread/3, get_thread/2, read_thread/3, delete_thread/2]).
-export([is_in_thread/2, thread_status/1]).
-export([create_post/3, read_post/3, delete_post/4]).

%% This module nicely (and relatively efficiently) wraps up all useful
%% basic database operations, so that other modules do not have to make
%% ad-hoc calls to mnesia. This is not a user interface. `soyuz_read`
%% and `soyuz_post` exist for that. All this module does beyond wrapping is
%% adjusting the appropriate metadata such as bumps and other dates, as well
%% as protecting from atom leaks (see the internal functions at the bottom
%% of this file). Tripcode processing, abstract page generation, et cetera,
%% all take place in the aformentioned `soyuz_read` and `soyuz_post`.

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
				mnesia:write(boards, Board, write);
			_ ->
				{board_already_exists, URI}
		end
	end,
	case mnesia:transaction(Fun) of
		{atomic, {board_already_exists, URI}} ->
			{board_already_exists, URI};
		{atomic, _} ->
			mnesia:create_table(
				threads_name(URI),
				[{attributes, record_info(fields, thread)},
				 {record_name, thread},
				 {index, [bump_date]}
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

%% Returns a thread listing sorted on bump order.
list_board(URI) ->
	Fun = fun() ->
		L = qlc:eval(qlc:q([P || P <- mnesia:table(threads_name(URI))])),
		F = fun(A, B) ->
			A#thread.bump_date > B#thread.bump_date
		end,
		lists:sort(F, L)
	end,
	transaction_board_protected(URI, Fun).

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
		create_post_unchecked(BoardURI, Thread, Post#post{
			date = Now
		})
	end,
	transaction_board_protected(BoardURI, Meat).

%%
get_thread(BoardURI, Threadno) ->
	Meat = fun() ->
		mnesia:read(threads_name(BoardURI), Threadno)
	end,
	transaction_board_protected(BoardURI, Meat).

%% Qualifier follows the model of various view types common to textboards.
%% This function returns a proplist with a thread and a list of its replies.
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
	transaction_thread_protected(BoardURI, Threadno, InnerMeat, read).

%% Deletes a thread from the threads table, as well as corresponding replies.
%% May be paired in future with an export function to allow for recovery.
delete_thread(BoardURI, Threadno) ->
	InnerMeat = fun(Result) ->
		mnesia:delete({threads_name(BoardURI), Result#thread.threadno}),
		qlc:eval(qlc:q([
			mnesia:delete({posts_name(BoardURI), P#post.threadno_replyno})
			|| P <- mnesia:table(posts_name(BoardURI)),
			   is_in_thread(P, Threadno)
		]))
	end,
	transaction_thread_protected(BoardURI, Threadno, InnerMeat, admin).

%% Does not actually touch the database; but belongs in this module
%% thematically. Simply checks if a `post` record has a given `Threadno`
%% within its `threadno_replyno` field. The pattern match to extract this
%% field is awkward, so this function is nice to have.
is_in_thread(Post, Threadno) ->
	{Threadno_, _} = Post#post.threadno_replyno,
	Threadno_ == Threadno.

%% Given the result of a mnesia query that should return exactly one thread,
%% either returns `ok` or an error.
thread_status(Result) ->
	case Result of
		[] ->
			no_such_thread;
		[#thread{closed = true}] ->
			thread_closed;
		[_Thread] ->
			ok;
		_ ->
			thread_collision
	end.

%% `Post` is a skeletal instance of the `post` record, with `date`
%% set if it is `undefined`. The one instance in which it is not
%% `undefined` upon reaching this function is thread creation, so that
%% the timestamp can exactly match the UNIX epoch time in the thread number.
create_post(BoardURI, Threadno, Post) ->
	InnerMeat = fun(Result) ->
		create_post_unchecked(BoardURI, Result, Post)
	end,
	transaction_thread_protected(BoardURI, Threadno, InnerMeat, post).

%% 
read_post(BoardURI, Threadno, Replyno) ->
	InnerMeat = fun(_) ->
		mnesia:read({posts_name(BoardURI), {Threadno, Replyno}})
	end,
	transaction_thread_protected(BoardURI, Threadno, InnerMeat, read).

%% If `Wipe` evaluates to true, the post is not simply marked as deleted,
%% but stripped of all its content. This is useful in case the post was
%% deleted for advertising illegal content, to avoid said links persisting
%% on the administrator's server(s).
delete_post(BoardURI, Threadno, Replyno, Wipe) ->
	InnerMeat = fun(_) ->
		case {
			mnesia:read({posts_name(BoardURI), {Threadno, Replyno}}),
			Wipe
		} of
			{[], _} ->
				{no_such_post, BoardURI, {Threadno, Replyno}};
			{[P], true} ->
				mnesia:write(posts_name(BoardURI), P#post{
					deleted = true,
					body = "",
					link = "",
					name = ""
				});
			{[P], false} ->
				mnesia:write(posts_name(BoardURI), P#post{deleted = true})
		end
	end,
	transaction_thread_protected(BoardURI, Threadno, InnerMeat, admin).

%% `threads_name/1` and `posts_name/1` generate atoms and hence would be
%% dangerous to export. The exported functions of this module,
%% wherever there would be a possiblity of generating bogus atoms,
%% eschew `mnesia:transaction/1` in favour of the two protected transaction
%% functions. The only place in which `mnesia:transaction/1` is during
%% board creation. Erlang style is heavily against defensive programming,
%% but this one exception must be made to guard against atom leak.

threads_name(Name) ->
	list_to_atom("threads_" ++ Name).

posts_name(Name) ->
	list_to_atom("posts_" ++ Name).

transaction_thread_protected(BoardURI, Threadno, InnerMeat, Mode) ->
	Meat = fun() ->
		Result = mnesia:read(threads_name(BoardURI), Threadno),
		case {thread_status(Result), Mode} of
			{ok, _}               -> InnerMeat(Result);
			{thread_closed, post} -> {thread_closed, post, BoardURI, Threadno};
			{thread_closed, _}    -> InnerMeat(Result);
			{Other, Mode}         -> {Other, Mode, BoardURI, Threadno}
		end
	end,
	transaction_board_protected(BoardURI, Meat).

transaction_board_protected(BoardURI, Meat) ->
	Fun = fun() ->
		case mnesia:read(boards, BoardURI) of
			[]  -> {no_such_board, BoardURI};
			[_] -> Meat()
		end
	end,
	mnesia:transaction(Fun).

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
	mnesia:write(posts_name(BoardURI), NewPost, write),
	mnesia:write(threads_name(BoardURI), NewThread, write).
