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
		"POST" -> real_do(ModData);
		_      -> {proceed, Data}
	end.

real_do(ModData) ->
	#mod {
		request_uri = RequestURI,
		entity_body = EntityBody
	} = ModData,
	Options = [{
		scheme_validation_fun,
		fun(Str) ->
			case Str of
				"http"  -> valid;
				"https" -> valid;
				_       -> {error, malformed_url}
			end
		end
	}],
	{
		_Scheme, _UserInfo, _Host, _Port,
		Path,
		_Query
	} = http_uri:parse(RequestURI, Options),
	Path = filename:split(http_uri:decode(Path)),
	Formdata = maps:from_list(cow_qs:decode_qs(list_to_binary(EntityBody))),
	case Path of
		["/", Board, "post"] ->
			{later};
		["/", Board, Threadno, "post"] ->
			F = fun(N) ->
				binary_to_list(maps:get(N, Formdata))
			end,
			soyuz_posting:post_thread(
				Board,
				Threadno,	
				F(<<"name">>),
				F(<<"link">>),
				F(<<"comment">>)
			)
	end.
	

	
	


	