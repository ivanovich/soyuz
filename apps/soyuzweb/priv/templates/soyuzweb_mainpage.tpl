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
<a name="newthread"></a>
<div id="createbox" class="outerbox">
	<div class="innerbox">
		<h2>New thread</h2>
		<form id="threadform" action="./post" method="post">
			<table>
				<tbody>
					<tr>
						<td>Title:</td>
						<td>
							<input name="title" size="46" maxlength="{{config.max_field_length}}" type="text">
							<input value="Create new thread" type="submit">
						</td>
					</tr>
					<tr>
						<td>Name:</td>
						<td>
							<input name="name" size="19" maxlength="{{config.max_field_length}}" type="text">
							Link:
							<input name="link" size="19" maxlength="{{config.max_field_length}}" type="text">
							<small>
								<a href="javascript:show('options')">More options...</a>
							</small>
						</td>
					</tr>
					<tr>
						<td></td>
						<td>
							<textarea name="comment" cols="64" rows="5" onfocus="size_field('threadform',15)" onblur="size_field('threadform',5)"></textarea>
						</td>
					</tr>
				</tbody>
			</table>
		</form>
	</div>
</div>
{% endblock %}