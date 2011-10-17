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

-module(cron_lib).

-export([name_sup/1, name_job_sup/1, name_job_srv/1]).

-include("../include/cron.hrl").
-include("zotonic.hrl").


name_sup(Context) ->
    name1(?CRON_SUP, Context).

name_job_sup(Context) ->
    name1(?CRON_JOB_SUP, Context).

name_job_srv(Context) ->
    name1(?CRON_JOB_SRV, Context).

name1(Name, #context{host=Host}) ->
    z_utils:name_for_host(Name, Host).

