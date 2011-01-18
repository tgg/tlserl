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
%% File    : DsLogAdmin_Common.erl
%% Purpose : Contains validation code used by the log and factory
%%           implementations.
%%----------------------------------------------------------------------

-module('DsLogAdmin_Common').

-export([check_full_action/1]).

%%----------------------------------------------------------------------
%% Include Files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include("DsLogAdmin.hrl").

check_full_action(Full_action) when is_integer(Full_action) ->
	Halt = 'DsLogAdmin':'halt'(),
	Wrap = 'DsLogAdmin':'wrap'(),
	case Full_action of
	    Halt -> ok;
	    Wrap -> ok;
	    _    -> corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	end;
check_full_action(_Full_action) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
