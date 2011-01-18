%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Thomas Girard <thomas.g.girard@free.fr> 2011.
%% All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
%%----------------------------------------------------------------------
%% File    : dsLogAdminSup.erl
%% Purpose : Contains supervisor specific callbacks.
%%----------------------------------------------------------------------

-module(dsLogAdminSup).
-behaviour(supervisor).

-export([start_link/1, init/1, create_link/3]).

start_link(_) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 3, 10},
	  [{"dsLogAdminChild",
	   {?MODULE, create_link, []},
	   transient, 100000, worker,
	   ['DsLogAdmin_BasicLogFactory',
	    'DsLogAdmin_Factory_impl']}]}}.

create_link(Module, Env, ArgList) ->
    Module:oe_create_link(Env, ArgList).
