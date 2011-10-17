{% if m.acl.is_admin %}
<li><a href="{% url admin_mod_cron %}" {% ifequal selected "admin_mod_cron" %}class="current"{% endifequal %}>Cron</a></li>
{% endif %}
