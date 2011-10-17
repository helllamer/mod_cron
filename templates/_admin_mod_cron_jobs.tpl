{% extends "admin_edit_widget_std.tpl" %}

{# Display status of current jobs #}

{% block widget_title %}Jobs{% endblock %}

{% block widget_content %}
<ul class="short-list">
    {% for job_id,job_task,job_pid,job_created  in  m.cron_status.show.served %}
	<li class="clearfix">
	    <span class="zp-10">{{ job_id|format_integer }}</span>
	    <span class="zp-60">{{ job_task }}</span>
	    <span class="zp-10">{{ job_pid }}</span>
	    <span class="zp-20">{{ job_created|date:"d.m.Y H:i" }}</span>
	</li>
    {% empty %}
	    <li>No tasks registered.</li>
    {% endfor %}
</ul>
{% endblock %}
