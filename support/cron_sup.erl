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

-module(cron_sup).

-behaviour(supervisor).

%% Internal API
-export([
	start_link/2,
	stop_all_jobs/1,
	start_job/3,
	stop_job/2,
	job_pids/1
    ]).

%% Supervisor callbacks
-export([ init/1 ]).

-include("../include/cron.hrl").

%% @doc to start two mod_cron supervisors.
start_link({Sup, _JobSup, _JobSrv} = Names, Context) ->
    supervisor:start_link({local, Sup}, ?MODULE, [sup, Names, Context]).


%% @doc Stop all running jobs. Forced restarting of level-2 supervisor causes all jobs to terminate.
stop_all_jobs(Context) ->
    [Sup, JobSup] = [cron_lib:name_sup(Context), cron_lib:name_job_sup(Context)],
    supervisor:restart_child(Sup, JobSup).


%% @doc launch a new job
start_job(JobId, Args, JobSup) ->
    ChildSpec = childdef_job(JobId, [Args]),
    case supervisor:start_child(JobSup, ChildSpec) of
	{ok, _Pid} = Result			-> Result;
	{ok, Pid, _Info}			-> {ok, Pid};
	{error, {already_started, _Pid}}	-> stop_job(JobId, JobSup),
						   start_job(JobId, Args, JobSup);
	{error, already_present}		-> supervisor:delete_child(JobSup, JobId),
						   start_job(JobId, Args, JobSup)
    end.


%% @doc stop the job by pid.
stop_job(JobId, JobSup) ->
    supervisor:terminate_child(JobSup, JobId).


%% @doc list all running job pids
job_pids(Context) ->
    JobSup = cron_lib:name_job_sup(Context),
    Children = supervisor:which_children(JobSup),
    lists:map(fun({_,Pid,_,_}) -> Pid end, Children).


%% @doc level-1 supervisor: used to manage simple-one-for-one level-2 supervisor
init([sup, {_, JobSup, JobSrv} = Names, Context]) ->
    {ok,
	%% rest_for_one strategy is used to synchronize job_srv and level-2 supervisor child-list.
	{_SupFlags = {rest_for_one, ?CRON_MAX_RESTART, ?CRON_MAX_TIME},
	    [
		%% job monitoring server
		{   JobSrv,
		    {cron_job_srv, start_link, [Names, Context]},
		    permanent,
		    2000,
		    worker,
		    [cron_job_srv]
		},

		%% simple-one-for-one supervisor is used for simple job-processes management
		{   JobSup,									%% ChildId
		    {supervisor, start_link, [{local, JobSup}, ?MODULE, job_sup]},		%% StartFun = {M, F, A}
		    permanent,									%% Restart  = permanent | transient | temporary
		    infinity,									%% Shutdown = brutal_kill | int() >= 0 | infinity
		    supervisor,									%% Type     = worker | supervisor
		    [?MODULE]									%% Modules  = [Module] | dynamic
		}
	    ]
	}
    };

%% @doc level-2 one-for-one supervisor.
%% Previously, simple_one_for_one was used, but due to compatibility problems (OTP-9201)
%% and other management issues, it was replaced by one_for_one.
init(job_sup) ->
    {ok,
	{_SupFlags = {one_for_one, ?CRON_MAX_RESTART, ?CRON_MAX_TIME},
	    []	  %% no children. They will be dynamically added later.
	}
    }.

%% @hidden generate child definition for level-2 supervisor.
childdef_job(JobId, Args) ->
    {
	JobId,
	{cron_thread, start_link, Args},
	permanent,
	brutal_kill,
	worker,
	[cron_thread]
    }.

