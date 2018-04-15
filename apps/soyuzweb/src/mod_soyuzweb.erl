-module(mod_soyuzweb).
%% Fancy specialised API module for inets. Existence of this file is
%% contingent upon this project not moving on to cowboy or something.
%% Does basic ``routing'' and handles page generation, posting, etc.

-include_lib("inets/include/httpd.hrl").

%% Callback API
-export([do/1]).

do(ModData) ->
	#mod {
		method = Method,
		data = Data
	} = ModData,
	case Method of
%%		"POST" -> realest_do(ModData);
		"POST" -> real_do(ModData);
		_      -> {proceed, Data}
	end.

%% Quick and dirty debug for development.
%%realest_do(ModData) ->
%%	F = (catch real_do(ModData)),
%%	{proceed, [{response,{200,lists:flatten(io_lib:format("~p", [F]))}}]}.

real_do(ModData) ->
	#mod {
		absolute_uri = AbsoluteURI,
		entity_body = EntityBody
	} = ModData,
	SV = fun(Str) ->
		case Str of
			"http"  -> valid;
			"https" -> valid;
			_       -> {error, AbsoluteURI}
		end
	end,
	%% For some absolutely twisted reason, inets httpd will, sometimes,
	%% provide an absolute URI lacking what is described in the pertinent
	%% documentation (it appears to take more stock in the raw client headers
	%% than the documentation would have us believe). This ugly hack greases
	%% the wheels in this case, and will have absolutely no effect whatsoever
	%% in the event that said httpd behaviour is ever considered a bug and
	%% fixed.
	MuhURI = case AbsoluteURI of
		"http://"  ++ _ -> AbsoluteURI;
		"https://" ++ _ -> AbsoluteURI;
		_               -> "http://" ++ AbsoluteURI
	end,
	{ok, {
		Scheme, _UserInfo, Host, Port,
		FlatPath,
		_Query
	}} = http_uri:parse(MuhURI, [{scheme_validation_fun, SV}]),
	Path = filename:split(http_uri:decode(FlatPath)),
	Formdata = maps:from_list(cow_qs:parse_qs(list_to_binary(EntityBody))),
	case Path of
		["/", Board, "post"] ->
			F = fun(N) ->
				binary_to_list(maps:get(N, Formdata))
			end,
			soyuz_posting:post_thread(
				Board,
				F(<<"title">>),
				F(<<"name">>),
				F(<<"link">>),
				F(<<"comment">>)
			),
			soyuzweb_pagegen:build_board(Board),
			Redirect = reconstruct(
				Scheme, Host, Port, ["/", Board]
			),
			{proceed, Redirect};
		["/", Board, Threadno, "post"] ->
			F = fun(N) ->
				binary_to_list(maps:get(N, Formdata))
			end,
			soyuz_posting:post_reply(
				Board,
				list_to_integer(Threadno),	
				F(<<"name">>),
				F(<<"link">>),
				F(<<"comment">>)
			),
			soyuzweb_pagegen:build_board(Board),
			Redirect = reconstruct(
				Scheme, Host, Port, ["/", Board]
			),
			{proceed, Redirect}
	end.

reconstruct(Scheme, Host, Port, Path) ->
	D = lists:flatten(io_lib:format("~s://~s:~B~s/", [
		Scheme, Host, Port, filename:join(Path)
	])),
	[{response, {response, [{code, 301}, {"Location", D}], "Nothing to see here; move along."}}].

	