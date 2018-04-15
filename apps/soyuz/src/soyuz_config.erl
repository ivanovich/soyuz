-module(soyuz_config).

-export([page_forward/2]).
-export([board_config_nodefault/1, board_config_nodefault/2]).
-export([board_config/1, board_config/2, combined_config/2]).

page_forward(Conf, App) ->
	[{A,V} ||
		{A,V} <- Conf,
		B <-
			application:get_env(soyuz, page_forwarded) ++
			application:get_env(App, page_forwarded),
		A == B
	].

%% board_config_nodefault(URI, soyuz). The same warning applies as for
%% board_config_nodefault/2.
board_config_nodefault(URI) ->
	board_config_nodefault(URI, soyuz).

%% The configuration of a specific board without any defaults. Not advised
%% for general use, but used internally for clarity and may have some
%% niche external use.
board_config_nodefault(URI, App) ->
	Lookup = fun(L) ->
		case proplists:lookup(URI, L) of
			none   -> [];
			{_, L} -> L
		end
	end,
	case application:get_env(App, boards) of
		undefined -> [];
		{ok, Val} -> Lookup(Val)
	end.

%% The configuration of a board in core `soyuz`, using calls to
%% `application:get_env/2`. The configuration for the specific board is
%% "overlaid" on top of the default board configuration (any keys found in
%% both prefer the specific; additional parameters are appended).
board_config(URI) ->
	board_config(URI, soyuz).

%% Like `board_config/1`, except for the corresponding configuration in the
%% same format for some application implementing a protocol for `soyuz`.
%%
%% Do not use this function if the protocol application in question does not
%% have or require extra configuration for board behaviour.
board_config(URI, App) ->
	Strip = fun(R) ->
		case R of
			undefined -> [];
			Other     -> Other
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
	{ok, D} = application:get_env(App, default_board),
	S = board_config_nodefault(URI, App),
	Overlay(Strip(D), Strip(S)).

%% Configuration for a board for core soyuz, with configuration for a
%% protocol application appended -- Appended, not overlaid. Protocol apps
%% should not override settings in core soyuz.
%%
%% As with `board_config/2`, do not use this particular function
%% if the protocol application in question does not have or require extra
%% configuration for board behaviour. However, if you are the developer of
%% such a protocol application and anticipate whatsoever the future need
%% for such a configuration, it may be well worth your time to pepper your
%% codebase with calls to combined_config(URI, yourapp) wherever you would
%% otherwise use board_config(URI), and then, and this is the most important
%% part, add {default_board, []} to your application's configuration. This
%% function checks to not inconsequentially append an empty list, so there
%% is next to overhead in the case of an empty protocol configuration.
combined_config(URI, ProtocolApp) ->
	case {board_config(URI), board_config(URI, ProtocolApp)} of
		{Core, []} -> Core;
		{Core, L}  -> Core ++ L
	end.