<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
	<title>{% block title %}{% endblock %}</title>
	<link rel="stylesheet" type="text/css" href="/static/blue_moon.css">
</head>
<body class="{% block bodyclass %}{% endblock %}">
	{% block content %}
	{% endblock %}
	<div id="footer">
		<a href="/">{{ sitename }}</a> - 
		<a href="https://www.youtube.com/watch?v=dQw4w9WgXcQ">RSS Feed</a> - 
		<a href="https://www.youtube.com/watch?v=dQw4w9WgXcQ">Soyuz {{version}}</a>
	</div>
</body>
</html>