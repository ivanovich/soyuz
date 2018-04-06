<div class="thread">
	<h2><a href="" rel="nofollow">{{thread.subject}}<span class="postcount">({{thread.post_count}})</span></a></h2>
	<div class="replies">
		<div class="allreplies">
		{% for post in posts %}
			<div class="reply">
				<h3>
					<span class="replynum">{{post.threadno_replyno}}</span>
					Name: 
					<span class="postername">{{post.name}}</span>
					<span class="postertrip">{{post.tripcode}}</span>
					: {{post.date}}
				</h3>
				<div class="replytext">{{post.body}}</div>
			</div>
		{% endfor %}
		</div>
	</div>
</div>