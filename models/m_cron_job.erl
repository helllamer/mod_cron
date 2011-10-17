%% @author Konstantin Nikiforov <helllamer@gmail.com>
%% @copyright 2011 Konstantin Nikiforov
%% @doc Display current mod_cron status

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

-module(m_cron_job).

-export([get_task/2, get_all/1, insert/3, update/3, delete/2, install/1]).

-include("../include/cron.hrl").
-include("zotonic.hrl").


%% get task from db -> undefined | {ok, Task}
get_task(JobId, Context) ->
    case z_db:q1("SELECT task FROM " ++ ?T_CRON_JOB ++ " WHERE id = $1", [JobId], Context) of
	undefined -> undefined;
	Task	  -> {ok, binary_to_task(Task)}
    end.

%% get all saved jobs
get_all(Context) ->
    Result = z_db:q("SELECT id, task FROM " ++ ?T_CRON_JOB, [], Context),
    lists:map(fun({JobId,Task}) -> {JobId, binary_to_task(Task)} end, Result).


%% insert/update task
insert(JobId, Task, Context) ->
    case get_task(JobId, Context) of
	undefined -> insert_nocheck(JobId, Task, Context);
	{ok, _}	  -> update(JobId, Task, Context)
    end.

insert_nocheck(JobId, Task, Context) ->
    Props = [
	{id,   JobId},
	{task, task_to_binary(Task)}
    ],
    {ok,_} = Result = z_db:insert(?T_CRON_JOB, Props, Context),
    z_notifier:notify({cron_job_inserted, JobId, Task}, Context),
    Result.

%% update task data for job
update(JobId, Task, Context) ->
    RowsUpdated = z_db:q1("UPDATE " ++ ?T_CRON_JOB ++ " SET task = $2 WHERE id = $1", [JobId, Task], Context),
    RowsUpdated > 0 andalso z_notifier:notify({cron_job_inserted, JobId, Task}, Context),
    {ok, RowsUpdated}.


%% @doc delete existing job
delete(JobId, Context) ->
    case z_db:delete(?T_CRON_JOB, JobId, Context) of
	{ok, RowsDeleted} = Result ->
	    RowsDeleted > 0 andalso z_notifier:notify({cron_job_deleted, JobId}, Context),
	    Result;
	E -> E
    end.

%% serialize/deserialize task data
task_to_binary(Task) when is_tuple(Task) ->
    term_to_binary(Task, [compressed]).

binary_to_task(Binary) when is_binary(Binary) ->
    binary_to_term(Binary);
binary_to_task(X) ->
    X.


%% create table if not exist
install(Context) ->
    z_db:ensure_table(?T_CRON_JOB, [
	    #column_def{name=id,   type="varchar", is_nullable=false},
	    #column_def{name=task, type="bytea",   is_nullable=false}
	], Context),
    % Add some constrains, ignore errors
    z_db:equery("ALTER TABLE " ++ ?T_CRON_JOB ++ " ADD PRIMARY KEY (id)", Context),
    ok.

