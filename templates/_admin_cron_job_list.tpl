{# render active jobs as list #}

<ul class="short-list">
    {% for job  in  m.cron_status.show.served %}
	<li class="clearfix">
	    <span class="zp-60"><strong>{{ job.id }}</strong> ({{ job.pid }})</span>
	    <span class="zp-20">{% button text=_"delete" postback={delete_job job_id=job.id} delegate="resource_cron_admin" %}</span>
	    <br/>
	    <span class="zp-100">{{ job.mfa }}</span>
	    <br/>
	    <span class="zp-50">Created: {{ job.created|date:"d.m.Y H:i" }}</span>
	    <span class="zp-50">Next run: {{ job.nextrun|date:"d.m.Y H:i:s" }}</span>
	    <br/>
	    <span class="zp-95">When: {{ job.when }}</span>
	</li>
    {% empty %}
	    <li>No tasks registered.</li>
    {% endfor %}
</ul>

