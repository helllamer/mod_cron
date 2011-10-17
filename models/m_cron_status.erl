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

-module(m_cron_status).

-behaviour(gen_model).
-export([m_find_value/3, m_to_list/2, m_value/2]).

-include("../include/cron.hrl").
-include("zotonic.hrl").

m_find_value(Atom, #m{value=undefined} = M, _Context) when is_atom(Atom) ->
    M#m{value=Atom};
m_find_value(served,  #m{value=show}, Context) ->   %% m.this.show.served
    format_served(Context);
m_find_value(running,  #m{value=show}, Context) ->  %% m.this.show.running
    format_running(Context);
m_find_value(_, _M, _Context) ->
    undefined.

m_to_list(_M, _Context) ->
    [].

m_value(_M, _Context) ->
    undefined.


%% Pre-render list of cron_job_srv
format_served(Context) ->
    {ok, Jobs} = cron_job_srv:jobs(Context),
    F = fun(#job{id=Id, pid=Pid, task={When, {M,F,A}}, created=Created, nextrun_ts=NextrunTs}) ->
	    [{id,	Id},
	     {'when',	str(When)},
	     {mfa,	str_mfa(M,F,A)},
	     {pid,	str(Pid)},
	     {created,	Created},
	     {nextrun,	z_datetime:timestamp_to_datetime(NextrunTs)} ]
    end,
    lists:map(F, Jobs).


%% Format supervisor child pid list (debug).
format_running(Context) ->
    Pids = cron_sup:job_pids(Context),
    lists:map(fun(Pid) -> str(Pid) end, Pids).


str(Term) ->
    io_lib:format("~p", [Term]).


str_mfa(M,F,A) ->
    [ $[ | Args0] = lists:flatten(io_lib:format("~p", [A])),
    Args1 = lists:sublist(Args0, length(Args0) - 1),
    io_lib:format("~p:~p(" ++ Args1 ++ ").", [M,F]).

