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

%% internal API
-export([ start_job/3, stop_job/2 ]).

%% z_notifier pins
-export([
	observe_cron_job_inserted/2,
	observe_cron_job_deleted/2
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


%% message from m_cron_job about new job insertion. Add job to execution.
observe_cron_job_inserted({cron_job_inserted, JobId, Task}, Context) ->
    start_job(JobId, Task, Context).

observe_cron_job_deleted({cron_job_deleted, JobId}, Context) ->
    stop_job(JobId, Context).

%% @doc run a new job
start_job(JobId, Task, Context) ->
    call_ctx({start_job, JobId, Task}, Context).

%% @doc stop and remove job from execution queue
stop_job(JobId, Context) ->
    call_ctx({stop_job, JobId}, Context).
    

%% start this server
start_link(Args) when is_list(Args) ->
    Context = proplists:get_value(context, Args),
    m_cron_job:install(Context),
    Name   = cron_lib:name_mod(Context),
    Result = gen_server:start_link({local, Name}, ?MODULE, Args, []),
    %% add stored jobs to execution
    AddF = fun({JobId, Task}) -> ?MODULE:start_job(JobId, Task, Context) end,
    ok   = lists:foreach(AddF, m_cron_job:get_all(Context)),
    %% Return start_link return value.
    Result.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%

init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    Sup	    = cron_lib:name_sup(Context),
    JobSup  = cron_lib:name_job_sup(Context),
    JobSrv  = cron_lib:name_job_srv(Context),
    {ok, _} = cron_sup:start_link({Sup, JobSup, JobSrv}, Context),
    State   = #state{sup=Sup, job_sup=JobSup, job_srv=JobSrv, context=Context},
    {ok, State}.


handle_call({start_job, JobId, Task}, _From, #state{job_srv=JobSrv, context=Context} = State) ->
    Reply = case cron_job_srv:add_job(JobId, Task, JobSrv) of
	{ok, Pid} = Result ->
	    z_notifier:notify({cron_job_started, JobId, Task, Pid}, Context),
	    Result;

	E -> E
    end,
    {reply, Reply, State};

handle_call({stop_job, JobId}, _From, #state{job_srv=JobSrv, context=Context} = State) ->
    case cron_job_srv:delete_job(JobId, JobSrv) of
	{error,_} -> 
	    ?LOG("Your erlang is too old < R14B03, so I cannot stop the job (due to problem in the supervisor.erl; see OTP-9201). But you can restart the mod_cron...", []);
	_ -> ok
    end,
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


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%

call_ctx(Req, Context) ->
    ModSrv = name(Context),
    gen_server:call(ModSrv, Req).


name(Context) ->
    cron_lib:name_mod(Context).

