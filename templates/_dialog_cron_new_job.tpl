<h3>Please define time and Module:Function(Args)</h3>

{% wire id=#form type="submit"
	postback="insert_job"
	delegate="resource_cron_admin"
%}
<form id="{{ #form }}" method="POST" action="postback">

{% wire id="fill_example" action=[
	{set_value target="id"		value="console_echo_every_5_sec"},
	{set_value target="when"	value="{daily, {every,{5,sec},{between,{0,am},{11,59,pm}}}}"},
	{set_value target="module"	value="io"},
	{set_value target="function"	value="format"},
	{set_value target="args"	value="\"hello, world~n\", []"}
    ] %}
<a id="fill_example" href="">Fill example values!</a>


	<div>
		<input type="text" name="id" id="id" title="String. Example: my_job1" />
		<label for="id" class="left">Uniq id:</label>
	</div>

	<div>
		<input type="text" name="when" id="when" title="click '?' button to see examples"/>
		<label for="when" class="left">When:</lable>
		{% wire id="time_examples" action=[{dialog_close}, {dialog_open title="Cron time examples" template="_dialog_cron_time_cheatsheet.tpl"}] %}
		<a id="time_examples" href="">?</a>
	</div>


	<hr/>
	
	<div>
		<input type="text" name="module" id="module" title="Module name. For example: io"/>
		<label for="module" class="left">M:</label>

		<input type="text" name="function" id="function" title="This function will be called. For example: format"/>
		<label for="function" class="left">F:</label>

		<input type="text" name="args" id="args" title="This args will be passed to function. For example: &quot;hello, world!&quot;, []"/>
		<label for="args" class="left">A:</label>
	</div>

	<hr/>

	<div class="form-item clearfix">
		<button type="submit">{_ Create _}</button>
		{% button action={dialog_close} text=_"Cancel" %}
	</div>
</form>
