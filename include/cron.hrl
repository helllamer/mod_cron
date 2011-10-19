%% @author Konstantin Nikiforov <helllamer@gmail.com>
%% @copyright 2011 Konstantin Nikiforov
%% @doc Defines for mod_crone.

%% Copyright 2011 Konstantin Nikiforov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-ifndef(CRON_HRL).
-define(CRON_HRL, true).


%% register names of task-management supervisors
-define(CRON_SUP,			cron_sup).
-define(CRON_JOB_SUP,			cron_job_sup).	    %% Must be NOT equal to CRON_SUP.
-define(CRON_JOB_SRV,			cron_job_srv).

%% supervisors child restart policy
-define(CRON_MAX_RESTART,		5).		    %% No more than 5 restarts ...
-define(CRON_MAX_TIME,			60).		    %% ... per 60 seconds.

%% One job in job list (cron_srv).
-record(job, {
	id,				%% JobId - some identifier to manage the job
	task,				%% Taks definition - is term, parseable by cron_thread
	pid,				%% Pid of task
	created=erlang:localtime(),	%% When this task is added/started
	counter=0,			%% How many times this task has been executed?
	nextrun_ts			%% Next execution timestamp
    }).


-define(CRON_JOB_STATUS_RUNNING,	'running').
-define(CRON_JOB_STATUS_WAITING,	'waiting').


%% table for storing tasks
-define(T_CRON_JOB,			"cron_job").


-endif.
