%% @author Konstantin Nikiforov <helllamer@gmail.com>
%% @copyright 2011 Konstantin Nikiforov
%% @doc Working with task object. Task = {When, MFA}

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

%%% Cron task type definition. Originally written by "Cat's eye technologies" %%%
%% @type  task()       = {when(), mfa()}
%% @type  mfa()        = {module(), function(), args()}
%% @type  when()       = {daily, period()}
%%                     | {weekly, dow(), period()} 
%%                     | {monthly, dom(), period()}
%% @type  dow()        = mon | tue | wed | thu | fri | sat | sun
%% @type  dom()        = integer()
%% @type  period()     = time() | [time()] | {every, duration(), constraint()}
%% @type  duration()   = {integer(), hr | min | sec}
%% @type  constraint() = {between, time(), time()}
%% @type  time()       = {integer(), am | pm} | {integer(), integer(), am | pm}

-module(cron_task).

-export([
	new/0,	    new/2,
	set_mfa/4,  parse_mfa/3,
	set_when/2, parse_when/1
    ]).

%% @doc create empty task tuple.
new() ->
    {undefined, undefined}.

%% @doc create task tuple with data
new(When, MFA) ->
    {When, MFA}.

%% @doc replace MFA for task
set_mfa(M,F,A, {When, _}) ->
    new(When, {M,F,A}).

%% @doc replace When-clause from task
set_when(When, {_, MFA}) ->
    new(When, MFA).


%% @doc Parse Module-Function-Args combination
parse_mfa(M, F, A) ->
    M1 = parse_term(M),
    F1 = parse_term(F),
    A1 = parse_term(ensure_list(A)),
    {ok, {M1,F1,A1}}.


%% @doc Parse when clause from string.
parse_when(When) ->
    {ok, parse_term(When)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%

%%% Parsing routines: we need to properly parse user's input of "when" and "args".
%% erl_parse wants to see dot at the end. Add the dot:
ensure_dot_end(Str) ->
    case lists:last(Str) of
	$. -> Str;
	_  -> Str ++ "."
    end.

%% eralng terms parser. Wrapper around erl_scan and erl_parse.
parse_term(Str) ->
    Str1 = ensure_dot_end(Str),
    {ok, Tokens, _} = erl_scan:string(Str1),
    {ok, Term}	= erl_parse:parse_term(Tokens),
    Term.

ensure_list(Str) ->
    [ $[ | Str] ++ "]".

