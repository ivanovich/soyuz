-module(soyuzweb_config).

-export([board_config/1]).

board_config(URI) ->
	soyuz_config:combined_config(URI, soyuzweb).