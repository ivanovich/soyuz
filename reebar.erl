%% Since rebar.config.script uses file:script/2 which cannot accept
%% preprocessor directives, our rebar.config.script takes this module
%% and puts it through compile:file/1, to take record definitions from 
%% a header and put them into erlydtl_opts, so templates are equipped 
%% with record information. There's probably a more principled way to
%% do this, but it works and that's all I care about.

-module(reebar).
-export([conf/0]).
-include("apps/soyuz/include/soyuz_fields.hrl").

conf() ->
	{erlydtl_opts, [
		{source_ext, ".tpl"},
		{module_ext, "_tpl"},
		{record_info, [
			{post, record_info(fields, post)},
			{thread, record_info(fields, thread)},
			{board, record_info(fields, board)}
		]}
	]}.
