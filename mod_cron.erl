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
-mod_title("Cron").
-mod_description("A task scheduler.").

-behaviour(gen_server).
-mod_schema(1).

%% z_notifier pins
-export([
	observe_cron_job_insert/2,
	observe_cron_job_inserted/2,
	observe_cron_job_delete/2,
	observe_cron_job_deleted/2,
	manage_schema/2
    ]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% state of this server
-record(state, {
	context,	%% zotonic context
			%%% Registered names of:
	sup,		%% level-1 supervisor
	job_sup,	%% job supervisor
	job_srv		%% job server
    }).

-include("include/cron.hrl").
-include("include/zotonic.hrl").

%% Server will subscribe on this events on init() and unsubscribe on terminate().
-define(MOD_SRV_SUBSCRIBE, [cron_job_start, cron_job_stop]).


%% @doc External management: add job to db and start.
observe_cron_job_insert({cron_job_insert, JobId, Task}, Context) ->
    m_cron_job:insert(JobId, Task, Context).

%% @doc External management: remove job from db and stop.
observe_cron_job_delete({cron_job_delete, JobId}, Context) ->
    m_cron_job:delete(JobId, Context).


%% @doc message from m_cron_job about new job insertion. Add job to execution.
observe_cron_job_inserted({cron_job_inserted, JobId, Task}, Context) ->
    z_notifier:first({cron_job_start, JobId, Task}, Context).

%% @doc message from m_cron_job about job deletion. Remove job from execution.
observe_cron_job_deleted({cron_job_deleted, JobId}, Context) ->
    z_notifier:first({cron_job_stop, JobId}, Context).

%% @doc create db schema.
manage_schema(What, Context) ->
    m_cron_job:manage_schema(What, Context).


%% @doc start this server
start_link(Args) when is_list(Args) ->
    Context = proplists:get_value(context, Args),
    %% ensure db schema
    m_cron_job:install(Context),
    %% start this server
    Name   = cron_lib:name_mod(Context),
    Result = gen_server:start_link({local, Name}, ?MODULE, Args, []),
    %% collect stored jobs and run them
    AddF = fun({JobId, Task}) -> z_notifier:first({cron_job_start, JobId, Task}, Context) end,
    StoredJobs = m_cron_job:get_all(Context),
    ok   = lists:foreach(AddF, StoredJobs),
    %% Return start_link return value.
    Result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%

init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    %% Names of other servers/supervisors
    Sup	    = cron_lib:name_sup(Context),
    JobSup  = cron_lib:name_job_sup(Context),
    JobSrv  = cron_lib:name_job_srv(Context),
    %% start level-1 supervisor.
    {ok, _} = cron_sup:start_link({Sup, JobSup, JobSrv}, Context),
    %% Subscribe to z_notifier events.
    Self = self(),
    ok = lists:foreach(fun(E) -> z_notifier:observe(E, Self, Context) end, ?MOD_SRV_SUBSCRIBE),
    %% ok!
    State   = #state{sup=Sup, job_sup=JobSup, job_srv=JobSrv, context=Context},
    {ok, State}.


%% to start the cron job
handle_call({{cron_job_start, JobId, Task}, _}, _From, #state{job_srv=JobSrv, context=Context} = State) ->
    Reply = run_job1(JobId, Task, JobSrv, Context),
    {reply, Reply, State};

%% start previously stopped job. Task is undefined, so it will extracted from DB.
handle_call({{cron_job_start, JobId}, _}, _From, #state{job_srv=JobSrv, context=Context} = State) ->
    %% get task from DB and run the job.
    Reply = case m_cron_job:get_task(JobId, Context) of
	undefined  -> {error, not_found};
	{ok, Task} -> run_job1(JobId, Task, JobSrv, Context)
    end,
    {reply, Reply, State};

%% stop job process
handle_call({{cron_job_stop, JobId}, _}, _From, #state{job_srv=JobSrv, context=Context} = State) ->
    cron_job_srv:delete_job(JobId, JobSrv),
    z_notifier:notify({cron_job_stopped, JobId}, Context),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    {reply, {badarg, Req}, State}.


handle_cast(Req, State) ->
    ?LOG("~p:~p unknown cast: ~p", [?MODULE, ?LINE, Req]),
    {noreply, State}.


handle_info(Info, State) ->
    ?LOG("~p:~p unknown info: ~p", [?MODULE, ?LINE, Info]),
    {noreply, State}.


%% @doc Server stops. It is time to unsubscribe from z_notifier events.
terminate(_Reason, #state{context=Context}) ->
    Self = self(),
    lists:foreach(fun(E) -> z_notifier:detach(E, Self, Context) end, ?MOD_SRV_SUBSCRIBE).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%

run_job1(JobId, Task, JobSrv, Context) ->
    case cron_job_srv:add_job(JobId, Task, JobSrv) of
	{ok, Pid} = Result ->
	    z_notifier:notify({cron_job_started, JobId, Task, Pid}, Context),
	    Result;

	E -> E
    end.

