%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright © 2011, Thomas Girard <thomas.g.girard@free.fr>
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
%% File    : DsLogAdmin_Common.hrl
%% Purpose : Common definitions.
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Include Files
%%----------------------------------------------------------------------
-include("TimeBase.hrl").
-include("DsLogAdmin.hrl").

-ifndef(DSLOGADMIN_COMMON_HRL).
-define(DSLOGADMIN_COMMON_HRL, true).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(log_attributes,
	{id,
	 objref,
	 full_action,
	 max_size = 0,
	 qos = ['DsLogAdmin':'QoSNone'()],
	 max_record_life = 0,
	 administrative_state = unlocked,
	 forward_state = off,
	 interval = #'TimeBase_IntervalT'{lower_bound=0, upper_bound=0},
	 capacity_alarm_thresholds = [],
	 week_mask = []}).

-record(log_factory,
	{id,
	 objref}).

-endif.
