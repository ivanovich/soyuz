%% This is a companion module to rebar.config.script, going through
%% compile:file/1 to take record definitions from a header and
%% put them into erlydtl_opts, so templates may refer to records.
%% There's probably a more principled way to do this, but it works
%% and that's all I care about.

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