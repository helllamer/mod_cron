{% extends "admin_edit_widget_std.tpl" %}

{# Display status of current jobs #}

{% block widget_title %}Jobs{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
	<div id="div_job_list">
		{% include "_admin_cron_job_list.tpl" %}
	</div>
	{% button text=_"Add job..." action={dialog_open title=_"Add job" template="_dialog_cron_new_job.tpl"} %}
{% endblock %}
