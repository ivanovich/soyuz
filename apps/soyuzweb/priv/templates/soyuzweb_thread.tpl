<div class="thread">
	<h2><a href="" rel="nofollow">{{thread.subject}} <span class="postcount">({{thread.post_count}})</span></a></h2>
	<div class="replies">
		<div class="allreplies">
		{% for post in thread.posts %}
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
	<form id="postform{{thread.threadno}}" action="{{thread.threadno}}/reply" method="post">
		<table>
			<tbody>
				<tr>
					<td>Name:</td>
					<td>
						<input name="name" size="19" maxlength="100" type="text">
						Link:
						<input name="link" size="19" maxlength="100" type="text">
						<input value="Reply" type="submit">
						<small>
							<a href="javascript:show('options{{thread.threadno}}')">More options...</a>
						</small>
					</td>
				</tr>
				<tr>
					<td></td>
					<td>
						<textarea name="comment" cols="64" rows="5" onfocus="size_field('postform{{thread.threadno}}', 15)" onblur="size_field('postform{{thread.threadno}}',5)"></textarea>
					</td>
				</tr>
			</tbody>
		</table>
	</form>
</div>