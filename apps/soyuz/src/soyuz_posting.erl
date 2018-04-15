-module(soyuz_posting).

-export([post_thread/5, post_reply/5]).

-include_lib("soyuz/include/soyuz_fields.hrl").


post_thread(Board, Subject, NameField, LinkField, BodyField) ->
	Conf = soyuz_config:board_config(Board),
	case proplists:lookup(allow_threads, Conf) of
		{_, true} ->
			soyuz_db:create_thread(Board, Subject, #post{
				name = case NameField of
					"" ->
						{_, L} = proplists:lookup(anonymous, Conf),
						I = random:uniform(length(L)),
						lists:nth(I, L);
					O  -> O
				end,
				link = LinkField,
				body = BodyField
			});
		{_, false} -> {no_threads_allowed};
		none -> {board_misconfigured}
	end.

post_reply(Board, Threadno, NameField, LinkField, BodyField) ->
	Conf = soyuz_config:board_config(Board),
	case proplists:lookup(allow_replies, Conf) of
		{_, true} ->
			soyuz_db:create_post(Board, Threadno, #post{
				name = case NameField of
					"" ->
						{_, L} = proplists:lookup(anonymous, Conf),
						I = random:uniform(length(L)),
						lists:nth(I, L);
					O  -> O
				end,
				link = LinkField,
				body = BodyField
			});
		{_, false} -> {no_replies_allowed};
		none -> {board_misconfigured}
	end.