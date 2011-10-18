%% @author Konstantin Nikiforov <helllamer@gmail.com>
%% @copyright 2011 Konstantin Nikiforov
%% @doc Job manager from mod_cron. Mantaining and monitoring of job lists.

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

-module(cron_job_srv).

-behaviour(gen_server).

%% Internal API exports
-export([start_link/2, add_job/3, delete_job/2, update_job_info/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state
-record(state, {
	jobs,		%% KV-storage. K = job_id, V = #job{}
	mfa_pids,	%% Running now job MFAs. Usually, this list looks empty. [{pid, job_id}]
	sup,		%% Cron host level-1 supervisor
	job_sup,	%% Cron job supervisor
	job_srv,	%% Registered job server name
	context		%% zotonic context (needed for z_notifier)
    }).

-include("../include/cron.hrl").
-include("zotonic.hrl").


%%%%%%%%%%%%%%%%%%%%
%%% Internal API %%%

%% @doc Start job manager
start_link({_, _, JobSrv} = Names, Context) ->
    gen_server:start_link({local, JobSrv}, ?MODULE, [Names, Context], []).


add_job(JobId, Task, JobSrv) ->
    gen_server:call(JobSrv, {add_job, JobId, Task}).

delete_job(JobId, JobSrv) ->
    gen_server:call(JobSrv, {delete_job, JobId}).

update_job_info(JobId, Info, Srv) ->
    gen_server:cast(Srv, {update_job_info, JobId, Info}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%

init([{Sup, JobSup, JobSrv} = _Names, Context]) ->
    process_flag(trap_exit, true),
    z_notifier:observe(cron_jobs_list, self(), Context),
    z_notifier:observe(cron_job_get, self(), Context),
    State = #state{
	context = Context,
	job_srv	= JobSrv,
	sup	= Sup,
	job_sup	= JobSup,
	jobs	= orddict:new(),
	mfa_pids= orddict:new()
    },
    {ok, State}.


%% run a new job, if not already running
handle_call({add_job, JobId0, Task0}, _From, #state{job_sup=JobSup, job_srv=JobSrv, jobs=Jobs0} = State) ->
    %% Task0 is raw task. Task - is a postprocessed version, where aliases are replaced with valid values.
    Task  = postprocess_task(Task0, State),
    JobId = z_convert:to_binary(JobId0),
    ResultState = case lookup_job(JobId, Jobs0) of
	
	%% No job with such id is started. Start new job.
	undefined ->
	    {ok, JobPid} = cron_sup:start_job(JobId, [JobId, Task, JobSrv], JobSup),
	    Job	  = cron_job:new(JobId, Task0, JobPid),
	    Jobs1 = orddict:store(JobId, Job, Jobs0),
	    State#state{jobs=Jobs1};

	%% Same task is already here - ignore
	{ok, #job{pid=JobPid, task=Task} = _Job} ->
	    State;

	%% Task definition is changed for existing job. Stop old job and start new.
	{ok, Job} ->
	    stop_job1(JobId, JobSup),
	    {ok, JobPid} = cron_sup:start_job(JobId, [JobId, Task, JobSrv], JobSup),
	    Job1  = cron_job:set_job_pid(JobPid,
			cron_job:set_task(Task0, Job)
		    ),
	    Jobs1 = orddict:store(JobId, Job1, Jobs0),
	    State#state{jobs=Jobs1}

    end,
    {reply, {ok, JobPid}, ResultState};

%% Return current job list
handle_call({cron_jobs_list, _}, _From, #state{jobs=Jobs} = State) ->
    Result = orddict:fold(fun(_, Job, Acc0) -> [Job | Acc0] end, [], Jobs),
    {reply, {ok, Result}, State};

%% Return some job
handle_call({{cron_job_get, JobId},_}, _From, #state{jobs=Jobs} = State) ->
    JobId1 = z_convert:to_binary(JobId),
    Reply = lookup_job(JobId1, Jobs),
    {reply, Reply, State};

%% stop job and delete it from the job list
handle_call({delete_job, JobId}, _From, #state{jobs=Jobs0, job_sup=JobSup} = State) ->
    JobId1 = z_convert:to_binary(JobId),
    stop_job1(JobId1, JobSup),
    Jobs1  = orddict:erase(JobId1, Jobs0),
    State2 = State#state{jobs=Jobs1},
    {reply, ok, State2};

handle_call(Request, _From, State) ->
    {reply, {badarg, Request}, State}.


%% Job process reporting about time of next run (seconds after now).
handle_cast({job_nextrun_in_seconds, JobId, Seconds}, #state{jobs=Jobs0, context=Context} = State) ->
    UpdateF = fun(Job) -> 
	    NextRunTimestamp = current_timestamp() + Seconds,
	    z_notifier:notify({cron_job_nextrun, JobId, NextRunTimestamp}, Context),
	    cron_job:set_nextrun(NextRunTimestamp, Job)
    end,
    Jobs1 = orddict:update(JobId, UpdateF, Jobs0),
    {noreply, State#state{jobs=Jobs1}};

%% Some running job is being executed. Add pid+job_id into mfa_pids list.
handle_cast({job_running, JobId, MfaPid}, #state{mfa_pids=MfaPids0, context=Context, jobs=Jobs0} = State) ->
    %% To link with spawned process
    link(MfaPid),
    MfaPids1 = orddict:store(MfaPid, JobId, MfaPids0),
    z_notifier:notify({cron_job_executed, JobId}, Context),
    %% and update the job execution counter
    UpdateF = fun(Job) -> cron_job:inc_counter(Job) end,
    Jobs1   = orddict:update(JobId, UpdateF, Jobs0),
    {noreply, State#state{mfa_pids=MfaPids1, jobs=Jobs1}};

handle_cast(_Request, State) ->
    {noreply, State}.


%% Supervisor wants to stop me.
handle_info({'EXIT', _From, shutdown}, State) ->
    {stop, normal, State};

%% Linked job mfa is finished: remove it from mfa_pids.
handle_info({'EXIT', MfaPid, _Reason}, #state{mfa_pids=MfaPids0} = State) ->
    MfaPids1 = orddict:erase(MfaPid, MfaPids0),
    {noreply, State#state{mfa_pids=MfaPids1}};

handle_info(Info, State) ->
    ?LOG("~p:~p unknown info: ~p", [?MODULE, ?LINE, Info]),
    {noreply, State}.


%% @doc server terminating. Detach it from z_notifier.
terminate(_Reason, #state{context=Context}) ->
    z_notifier:detach(cron_jobs_list, self(), Context),
    z_notifier:detach(cron_job_get,   self(), Context).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%

%% lookup for existing task in the list of active tasks
lookup_job(JobId, Jobs) ->
    case orddict:find(JobId, Jobs) of
	{ok,_}=Result -> Result;
	_	      -> undefined
    end.


%% Stop the job and return a new job list.
stop_job1(JobId, JobSup) ->
    cron_sup:stop_job(JobId, JobSup).


current_timestamp() ->
    z_datetime:datetime_to_timestamp(erlang:localtime()).


%% any filters for postprocessing the task body.
postprocess_task(Task, State) ->
    postprocess_args(Task, State).

%% replace arg aliases with expected values
postprocess_args({When, {M,F,Args0}}, State) ->
    Args1 = postprocess_args1(Args0, State),
    {When, {M,F,Args1}}.

%% use dummy context.
%% @todo Add more aliases!
postprocess_args1(['context@' | T], State) ->
    [State#state.context | postprocess_args1(T, State)];
%% context with admin rights
postprocess_args1(['context_sudo@' | T], State) ->
    ContextSudo = z_acl:sudo(State#state.context),
    [ContextSudo | postprocess_args1(T, State)];
%% inner list of args (not string) should be also postprocessed
postprocess_args1([ [H|_]=List | T], State) when not is_integer(H) ->
    [postprocess_args1(List,State) | postprocess_args1(T,State)];
%% other args are skipped
postprocess_args1([H|T], State) ->
    [H | postprocess_args1(T, State)];
postprocess_args1([], _State) ->
    [].

