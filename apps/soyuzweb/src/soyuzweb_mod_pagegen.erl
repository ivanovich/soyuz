-module(soyuzweb_mod_pagegen).
%% Fancy specialised API module for inets. Existence of this file is
%% contingent upon this project not moving on to cowboy or something.
%% Does basic ``routing'' and handles page generation, posting, etc.

-include_lib("inets/include/httpd.hrl").

%% Callback API
-export([do/1]).

do(ModData) ->
	{proceed, ModData#mod.data}.