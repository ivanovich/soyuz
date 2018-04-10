-module(soyuz_dev).

-compile(export_all).
-include_lib("soyuz/include/soyuz_fields.hrl").

populate() ->
	[
		soyuz_db:setup(),
		soyuz_db:create_board("b", "Random", "<p>Works herein, fiction falshood, yadda yadda.</p>"),
		soyuz_db:create_thread(
			"b",
			"owo whats this",
			#post {
				name = "Anonymous",
				link = "yuuko@cock.li",
				body = "im 12 and what is this"
			}
		)
	].
		