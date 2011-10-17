{% extends "admin_base.tpl" %}

{% block title %}mod_cron{% endblock %}

{% block content %}
<div id="content" class="zp-85">
    <div class="block clearfix">
	<h2>mod_cron - cron-like task management</h2>

	{# currently running jobs: #}
	{% include "_admin_mod_cron_jobs.tpl" %}
    </div>
</div>
{% endblock %}
