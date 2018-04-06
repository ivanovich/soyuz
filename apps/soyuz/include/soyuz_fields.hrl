%% one table like this in the entire node
-record(board, {
	name,
	title,
	header
}).

%% one table per board
-record(thread, {
	id,
	subject,
	permasage,
	closed,
	bump_date,
	post_count
}).

%% one table per board
-record(post, {
	threadno_replyno, % tuple
	deleted,
	date, % decided server side
	name, % decided client side
	tripcode,
	link, % decided client side
	body  % decided client side
}).