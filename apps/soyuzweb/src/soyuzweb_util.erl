-module(soyuzweb_util).
-export([doc_root/0]).

doc_root() ->
	Pid = proplists:get_value(httpd, inets:services()),
	[
		{server_root, ServerRoot},
		{document_root, DocumentRoot}
	] = httpd:info(Pid, [server_root, document_root]),
	[ServerRoot, DocumentRoot].