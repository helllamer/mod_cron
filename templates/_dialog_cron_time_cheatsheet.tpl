<strong>mod_cron</strong> {_ will call M:F(A) every... _}
<br/><hr/>

<strong>{_ At 3:30 PM every day: _}</strong>
<br/>
{daily, {3, 30, pm}}
<hr/>

<strong>{_ Every 5 seconds all-day: _}</strong>
<br/>
{daily,{every,{5,sec},{between,{0,am},{11,59,pm}}}}
<hr/>

<strong>{_ Every week at Thursday at 2:00 AM: _}</strong>
<br/>
{weekly, thu, {2, am}}
<hr/>

<a href="http://catseye.tc/projects/crone/src/crone_test.erl" target="_blank">See more examples</a> (go and scroll down by 50%)...
