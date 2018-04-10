%% one table like this in the entire node
-record(board, {
	uri,
	title,
	header
}).

%% one table per board
-record(thread, {
	threadno,
	subject,
	permasage,
	closed,
	bump_date,
	post_count
}).

%% one table per board
-record(post, {
	threadno_replyno, % 2-tuple
	deleted = false,
	date, % decided server side
	name = "Anonymous", % decided client side
	trip = "",
	link = "", % decided client side
	body % decided client side
}).