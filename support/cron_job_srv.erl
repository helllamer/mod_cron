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
-export([start_link/1, add_job/3, delete_job/2, update_job_info/3, list/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state
-record(state, {
	context,	%% zotonic context
	jobs,		%% KV-storage. K = job_id, V = #job{}
	run,		%% Running now job MFAs. Usually, this list looks empty. [{pid, job_id}]
	name		%% Registered module name
    }).

-include("../include/cron.hrl").


%%%%%%%%%%%%%%%%%%%%
%%% Internal API %%%

%% @doc Start job manager
start_link(Context) ->
    process_flag(trap_exit, true),
    SrvName = name(Context),
    gen_server:start_link({local, SrvName}, ?MODULE, [SrvName, Context], []).


add_job(JobId, Task, Context) ->
    call_ctx({add_job, JobId, Task}, Context).

delete_job(JobId, Context) ->
    call_ctx({delete, JobId}, Context).

update_job_info(JobId, Info, Srv) ->
    gen_server:cast(Srv, {update_job_info, JobId, Info}).

list(Context) ->
    call_ctx(list, Context).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%

init([SrvName, Context]) ->
    State = #state{
	name	= SrvName,
	context = Context,
	jobs	= orddict:new(),
	pid	= orddict:new()
    },
    {ok, State}.


%% run a new job, if not already running
handle_call({add_job, JobId, Task}, _From, #state{context=Context, jobs=Jobs0} = State) ->
    ResultState = case lookup_job(JobId, Jobs0) of
	
	%% Same task is already here - ignore
	{ok, #job{pid=JobPid, task=Task} = _Job} ->
	    State;

	%% Task definition is changed for existing job. Stop old job and start new.
	{ok, Job} ->
	    stop_job(Job, Context),
	    {ok, JobPid} = start_job(JobId, Task, Context),
	    Job1  = Job#job{pid=JobPid, task=Task},
	    Jobs1 = add_job1(JobId, Job1, Jobs0),
	    State#state{jobs=Jobs1};

	%% No job started. Just start it.
	undefined ->
	    {ok, JobPid} = start_job(JobId, Task, Context),
	    Jobs1 = add_job1(JobId, Task, Pid, Jobs0),
	    State#state{jobs=Jobs1}

    end,
    {reply, {ok, JobPid}, ResultState};

%% Return current job list
handle_call(list, _From, #state{jobs=Jobs} = State) ->
    {reply, {ok, Jobs}, State};

%% stop job and delete it from the job list
handle_call({delete_job, JobId}, _From, #state{jobs=Jobs0, context=Context} = State) ->
    {Reply, State2} = case lookup_job(JobId, Jobs0) of
	{ok, Job} ->
	    stop_job(Job, Context),
	    Jobs1 = delete_job1(JobId, Jobs0),
	    {ok, State#state{jobs=Jobs1}};

	_-> {undefined, State}
    end,
    {reply, Reply, State2};

handle_call(Request, _From, State) ->
    {reply, {badarg, Request}, State}.


%% Some running job is reporting activity
handle_cast({update_job_info, JobId, Info}, #state{jobs=Jobs} = State) ->
    Jobs1 = update_job(JobId, Info, Jobs),
    {noreply, State#state{jobs=Jobs1}};

handle_cast(_Request, State) ->
    {noreply, State}.


%% Attached job subprocess is finished.
handle_info({'EXIT', FromPid, _Reason}, #state{jobs=Jobs} = State) ->
    Jobs1 = set_status_lastpid(FromPid, ?CRON_JOB_STATUS_WAITING, Jobs),
    {noreply, State#state{jobs=Jobs1}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%

%% name of server depends on host running.
name(Context) ->
    cron_lib:name_job_srv(Context).


%% lookup for existing task in the list of active tasks
lookup_job(JobId, Jobs) ->
    case orddict:find(JobId, Jobs) of
	{ok,_}=Result -> Result;
	_	      -> undefined
    end.

%% Stop the job and return a new job list.
stop_job(#job{pid=Pid} = Job, Context) ->
    cron_sup:stop_job(Pid, Context).

delete_job1(JobId, Jobs) ->
    orddict:erase(JobId, Jobs).

%% Start a new job and add it to list -> {ok, Pid, Jobs2}.
start_job(JobId, Task, Context) ->
    {ok, Pid} = cron_sup:start_job(Task, Context),
    {ok, Pid}.


add_job1(JobId, Task, Pid, Jobs) ->
    Job = #job{task=Task, pid=Pid},
    add_job1(JobId, Job, Jobs).

add_job1(JobId, Job, Jobs) ->
    orddict:store(JobId, Job, Jobs).


%% gen_server:call wrapper for dynamically named server
call_ctx(Message, Context) ->
    gen_server:call(name(Context), Message).


%% update job in the job list
update_job(JobId, Info, Jobs) ->
    UpdateF = fun(Job) -> update_job1(Info, Job) end,
    orddict:update(JobId, UpdateF, Jobs).


%% update job information (~patch #job{} record)
update_job1([{cycle, LastPid}|T], #job{counter=Counter} = Job) ->
    Job1 = Job#job{
	counter  = Counter+1,
	last_pid = LastPid,
	status   = ?CRON_JOB_STATUS_RUNNING
    },
    %% attach to new job subproccess (maybe, already finished process)
    erlang:link(LastPid),
    update_job1(T, Job1);
update_job1([{next_run_in_seconds, Seconds}|T], Job) ->
    NextRunTimestamp = current_timestamp() + Seconds,
    update_job1(T, Job#job{next_run_ts=NextRunTimestamp});
update_job1([{task, Task}|T], Job) ->
    update_job1(T, Job#job{task=Task});
update_job1([{pid, Pid}|T], Job) ->
    update_job1(T, Job#job{pid=Pid});
update_job1([], Job) ->
    Job.


%% Find job using last_pid and set status to Status
set_status_lastpid(LastPid, Status, [#job{last_pid=LastPid}=Job|T]) ->
    [Job#job{status=Status} | T];
set_status_lastpid(LastPid, Status, [H|T]) ->
    [H | set_status_lastpid(LastPid, Status, T)];
set_status_lastpid(_, _, []) ->	%% silently ignore strange last_pid
    [].


current_timestamp() ->
    z_datetime:datetime_to_timestamp(erlang:localtime()).
