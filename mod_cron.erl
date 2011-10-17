%% @author Konstantin Nikiforov <helllamer@gmail.com>
%% @copyright 2011 Konstantin Nikiforov
%% @doc Cron-like job manager extension for zotonic.

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

-module(mod_cron).
-author("Konstantin Nikiforov <helllamer@gmail.com>").
-mod_title("cron").
-mod_description("A task scheduler.").

-export([
	init/1,
	add_job/3
    ]).

%% z_notifier pins
-export([observe_cron_add_job/2]).

-include("include/cron.hrl").


observe_cron_add_job({cron_add_job, JobId, Task}, Context) ->
    cron_job_srv:add_job(JobId, Task, Context).


%% initialize the module.
init(Context) ->
    cron_sup:start_link(Context),
    start_all_jobs(Context).


%% @doc dynamically run a new job
add_job(JobId, Task, Context) ->
    case cron_job_srv:add_job(JobId, Task, Context) of
	{ok, Pid} = Result ->
	    z_notifier:notify({cron_job_added, JobId, Task, Pid}, Context),
	    Result;
	E -> E
    end.

%% @doc broadcast for the jobs. FIXME Function does not check for duplicate tasks!
start_all_jobs(Context) ->
    Jobs = z_notifier:foldl(cron_collect_jobs, [], Context),
    lists:foreach(fun({JobId, Task}) -> add_job(JobId, Task, Context) end, Jobs).

%% @doc Remove all jobs.
%stop_all_jobs(Context) ->
%    cron_sup:stop_all_jobs(Context).

