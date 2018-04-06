{% extends "soyuzweb_page.tpl" %}
{% block bodyclass %}threadpage{% endblock %}
{% block content %}
<div id="posts">
	<div class="thread">
		{% include "soyuzweb_thread.tpl" %}
	</div>
</div>
{% endblock %}