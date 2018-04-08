-module(soyuz_dev).

-compile(export_all).

populate() ->
	[
		soyuz_db:setup(),
		soyuz_db:create_board("b", "Random", "<p>Works herein, fiction falshood, yadda yadda.</p>"),
		soyuz_db:make_thread("b", "owo whats this", {"Anonymous", "yuuko@cock.li", "im 12 and what is this"})
	].
		