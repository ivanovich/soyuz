-module(soyuz_config).

-export([board_config/1, board_config/2, combined_config/2]).

board_config(URI) ->
	board_config(URI, soyuz).

board_config(URI, App) ->
	Strip = fun(R) ->
		case R of
			undefined  -> [];
			{ok, Other} -> Other
		end
	end,
	Overlay = fun(Dst, Src) ->
		Insert = fun({Key, Term}, AccDst) ->
			case proplists:lookup(Key, AccDst) of
				none ->
					AccDst ++ [{Key, Term}];
				{Key, _} ->
					Replace = fun({Candidate, OldTerm}) ->
						case Candidate == Key of
							true -> {Key, Term};
							false -> {Candidate, OldTerm}
						end
					end,
					lists:map(Replace, AccDst)
			end
		end,
		lists:foldl(Insert, Dst, Src)
	end,
	D = application:get_env(App, default_board),
	S = application:get_env(App, boards),
	Overlay(Strip(D), Strip(S)).

combined_config(URI, ProtocolApp) ->
	board_config(URI) ++ board_config(URI, ProtocolApp).