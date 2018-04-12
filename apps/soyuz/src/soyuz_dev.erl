-module(soyuz_dev).

-compile(export_all).
-include_lib("soyuz/include/soyuz_fields.hrl").

populate() ->
	[
		soyuz_db:setup(),
		soyuz_db:create_board("b", "Random", "<p>Works herein, fiction falshood, yadda yadda.</p>"),
		soyuz_db:create_thread(
			"b",
			"hi im 12 and what is this",
			#post {
				name = "Anonymous",
				link = "mailto:terrydavis@cia.gov",
				body = "please do not report me i am being tracked by handsome glow in the dark gentlemen"
			}
		)
	].
		