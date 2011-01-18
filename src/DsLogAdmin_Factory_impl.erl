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
%% File    : DsLogAdmin_Factory_impl.erl
%% Purpose : Implementation of DsLogAdmin:BasicLogFactory.
%%----------------------------------------------------------------------

-module('DsLogAdmin_Factory_impl').

-include_lib("orber/include/corba.hrl").
-include("DsLogAdmin.hrl").

-export([create/4, create_with_id/5]).
-export([list_logs/2, find_log/3, list_logs_by_id/2]).

%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%======================================================================
%% API Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : create/4
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Full_action = unsigned_Short()
%%              Max_size = unsigned_Long_Long()
%% Returns    : ReturnValue = {OE_Reply, Id}
%%              OE_Reply = Object_Ref()
%%              Id = unsigned_Long()
%% Raises     : DsLogAdmin::InvalidLogFullAction
%% Description: 
%%----------------------------------------------------------------------
create(OE_This, State, Full_action, Max_size) ->
	Id = next_id(State),
	OE_Reply = new_log(OE_This, Id, Full_action, Max_size),
	{reply, {OE_Reply, Id}, [OE_Reply | State]}.

%%----------------------------------------------------------------------
%% Function   : create_with_id/5
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Id = unsigned_Long()
%%              Full_action = unsigned_Short()
%%              Max_size = unsigned_Long_Long()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = Object_Ref()
%% Raises     : DsLogAdmin::LogIdAlreadyExists
%%              DsLogAdmin::InvalidLogFullAction
%% Description: 
%%----------------------------------------------------------------------
create_with_id(OE_This, State, Id, Full_action, Max_size) ->
	case lookup(State, Id) of
	    [] -> OE_Reply = new_log(OE_This, Id, Full_action, Max_size),
		  {reply, {OE_Reply, Id}, [OE_Reply | State]};
	    _  -> corba:raise(#'DsLogAdmin_LogIdAlreadyExists'{})
	end.

%%----------------------------------------------------------------------
%% Function   : list_logs/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = Object_Ref()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
list_logs(_OE_This, State) ->
	{reply, State, State}.

%%----------------------------------------------------------------------
%% Function   : find_log/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Id = unsigned_Long()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = Object_Ref()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
find_log(_OE_This, State, Id) ->
	OE_Reply = case lookup(State, Id) of
		       []    -> corba:create_nil_objref();
		       [H|_] -> H
		   end,
	{reply, OE_Reply, State}.

%%----------------------------------------------------------------------
%% Function   : list_logs_by_id/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = unsigned_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
list_logs_by_id(_OE_This, State) ->
	OE_Reply = lists:map(fun(L) -> 'DsLogAdmin_Log':id(L) end, State),
	{reply, OE_Reply, State}.


%%======================================================================
%% Internal Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Arguments  : Env = term()
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Raises     : -
%% Description: Initiates the server
%%----------------------------------------------------------------------
init(_Env) ->
	{ok, []}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Arguments  : Reason = normal | shutdown | term()
%%              State = term()
%% Returns    : ok
%% Raises     : -
%% Description: Invoked when the object is terminating.
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Arguments  : OldVsn = undefined | term()
%%              State = NewState = term()
%%              Extra = term()
%% Returns    : {ok, NewState}
%% Raises     : -
%% Description: Invoked when the object should update its internal state
%%              due to code replacement.
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%% Function   : handle_info/2
%% Arguments  : Info = normal | shutdown | term()
%%              State = NewState = term()
%% Returns    : {noreply, NewState}          |
%%              {noreply, NewState, Timeout} |
%%              {stop, Reason, NewState}
%% Raises     : -
%% Description: Invoked when, for example, the server traps exits.
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

next_id([])    -> 1;
next_id(Logs)  -> 1 + lists:max(lists:map(fun(Log) -> 'DsLogAdmin_Log':id(Log) end, Logs)).

lookup(Logs, Id) when is_integer(Id) andalso Id >= 0 ->
	lists:filter(fun(L) -> 'DsLogAdmin_Log':id(L) =:= Id end, Logs);
lookup(_Logs, _Id) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    
check_max_size(Max_size) when is_integer(Max_size) andalso Max_size >= 0 ->
	ok;
check_max_size(_Max_size) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

new_log(OE_This, Id, Full_action, Max_size) ->
    'DsLogAdmin_Common':check_full_action(Full_action),
    check_max_size(Max_size),
    'DsLogAdmin_BasicLog':oe_create([OE_This, Id, Full_action, Max_size]).
