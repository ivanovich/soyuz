{% extends "soyuzweb_page.tpl" %}
{% block title %}{{board.title}}{% endblock %}
{% block bodyclass %}mainpage{% endblock %}
{% block content %}
<div id="titlebox" class="outerbox">
	<div class="innerbox">
		<h1>{{board.title}}</h1>
		<div id="rules">
			{% autoescape off %}
			{{board.header}}
			{% endautoescape %}
		</div>
	</div>
</div>
<a name="menu"></a>
<div id="threadbox" class="outerbox">
	<div class="innerbox">
		<div id="threadlist">
		{% for thread in index %}
			<span class="threadlink">
			{% if forloop.counter <= 10 %}
				<a href="{{thread.id}}">{{forloop.counter}}: </a>
				<a href="#{{forloop.counter}}">{{thread.subject}} ({{thread.post_count}})</a>
			{% else %}
				<a href="{{thread.id}}">{{forloop.counter}}: {{thread.subject}} ({{thread.post_count}})</a>
			{% endif %}
			</span>
		{% endfor %}
		</div>
	</div>
</div>
<div id="posts">
{% for thread in threads %}
	<a name="{{forloop.counter}}"></a>
	{% include "soyuzweb_thread.tpl" %}
{% endfor %}
</div>
{% endblock %}