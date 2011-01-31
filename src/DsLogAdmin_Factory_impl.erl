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
%% File    : DsLogAdmin_Factory_impl.erl
%% Purpose : Implementation of DsLogAdmin:BasicLogFactory.
%%----------------------------------------------------------------------
%% Table   : oe_tlsbf
%% Purpose : contains the list of all LogMgr implementations, along
%%           with the associated object reference:
%%
%%  +------------+------------------+
%%  | Factory Id | Object reference |
%%  +------------+------------------+
%%  |            |                  |
%%  +------------+------------------+
%%
%%----------------------------------------------------------------------
%% Table   : oe_tlsbf_n_log_attr
%% Purpose : contains the log ids and attributes for log factory n:
%%
%%  +----+-----+------+------+-----+------+-----+-----+-----+-----+----+
%%  | Id | Log | Full | M sz | QoS | M lf | Adm | Fwd | Int | Thr | Wk |
%%  +----+-----+------+------+-----+------+-----+-----+-----+-----+----+
%%  |    |     |      |      |     |      |     |     |     |     |    |
%%  +----+-----+------+------+-----+------+-----+-----+-----+-----+----+
%%
%%  * Id   -- the log id
%%  * Log  -- the log object reference
%%  * Full -- the action to perform when the log is full
%%  * M sz -- the max size of the log
%%  * QoS  -- QoS settings
%%  * M lf -- maximum life of log records
%%  * Adm  -- administrative state of the log
%%  * Fwd  -- forwarding state of the log
%%  * Int  -- log interval
%%  * Thr  -- capacity alarm thresholds
%%  * Wk   -- week mask
%%
%%----------------------------------------------------------------------

-module('DsLogAdmin_Factory_impl').

%%----------------------------------------------------------------------
%% Include Files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("DsLogAdmin.hrl").
-include("DsLogAdmin_Common.hrl").

%%----------------------------------------------------------------------
%% API Exports
%%----------------------------------------------------------------------
-export([create/4, create_with_id/5]).
-export([list_logs/2, find_log/3, list_logs_by_id/2]).

%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state,
	{id,
	 table_name}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(LOG_ATTR_NAME(S), S#state.table_name).
-define(LOG_ATTR_TABLE(S), mnesia:table(S#state.table_name)).

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
%% Description: Creates a new log
%%----------------------------------------------------------------------
create(OE_This, State, Full_action, Max_size) ->
    Id = next_log_id(State),
    OE_Reply = new_log(OE_This, State, Id, Full_action, Max_size),
    {reply, {OE_Reply, Id}, State}.

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
%% Description: Creates a new log with id `Id'
%%----------------------------------------------------------------------
create_with_id(OE_This, State, Id, Full_action, Max_size) ->
    case lookup(State, Id) of
	[] ->
	    OE_Reply = new_log(OE_This, State, Id, Full_action, Max_size),
	    {reply, {OE_Reply, Id}, State};
	_  ->
	    corba:raise(#'DsLogAdmin_LogIdAlreadyExists'{})
    end.

%%----------------------------------------------------------------------
%% Function   : list_logs/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = Object_Ref()
%% Raises     : 
%% Description: Retrieves all existing logs associated to this log
%%              manager
%%----------------------------------------------------------------------
list_logs(_OE_This, State) ->
    Query = qlc:q([L#log_attributes.objref || L <- ?LOG_ATTR_TABLE(State)]),
    Logs = 'DsLogAdmin_Common':do(Query),
    {reply, Logs, State}.

%%----------------------------------------------------------------------
%% Function   : find_log/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Id = unsigned_Long()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = Object_Ref()
%% Raises     : 
%% Description: Retrieves log with id `Id'
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
%% Description: Retrieves all logs id
%%----------------------------------------------------------------------
list_logs_by_id(_OE_This, State) ->
    Query = qlc:q([L#log_attributes.id || L <- ?LOG_ATTR_TABLE(State)]),
    OE_Reply = 'DsLogAdmin_Common':do(Query),
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
%% Description: Initiates server, creating oe_tlsbf table if needed.
%%----------------------------------------------------------------------
init(Env) ->
    case get_log_mgr_id(Env) of
	{ok, Id} ->
	    new_log_mgr(Id);
	{error, Reason} ->
	    {stop, Reason}
    end.

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

next_log_id(Record, Max) ->
    max(Record#log_attributes.id, Max).

next_log_id(#state{table_name=TableName}) ->
    F = fun () -> 1 + mnesia:foldl(fun next_log_id/2, 0, TableName) end,
    {atomic, Id} = mnesia:transaction(F),
    Id.

lookup(State, Id) when is_integer(Id) andalso Id >= 0 ->
    F = fun () -> mnesia:read(?LOG_ATTR_NAME(State), Id) end,
    {atomic, Val} = mnesia:transaction(F),
    Val;
lookup(_State, _Id) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

new_log(OE_This, #state{id=Fid, table_name=Name}, Id, FullAction, MaxSize) ->
    'DsLogAdmin_Common':check_full_action(FullAction),
    'DsLogAdmin_Common':check_max_size(MaxSize),
    Attr = #log_attributes{id=Id, full_action=FullAction, max_size=MaxSize},
    'DsLogAdmin_BasicLog':oe_create([{attributes, Attr},
				     {table_name, Name},
				     {factory, Fid, OE_This}]).

next_log_mgr_id(Record, Max) ->
    max(Record#log_factory.id, Max).

next_log_mgr_id() ->
    F = fun () -> 1 + mnesia:foldl(fun next_log_mgr_id/2, 0, oe_tlsbf) end,
    {atomic, Id} = mnesia:transaction(F),
    Id.

log_attr_table_name(Id) ->
    list_to_atom("oe_tlsbf_" ++ integer_to_list(Id) ++ "_log_attr").

create_log_attr_table(Id) ->
    LogAttrName = log_attr_table_name(Id),
    case 'DsLogAdmin_Common':create_table(LogAttrName,
					  record_info(fields, log_attributes),
					  log_attributes) of
	{ok, {_, LogAttrName}} ->
	    {ok, LogAttrName};
	{error, Reason} ->
	    {error, Reason}
    end.

new_log_mgr(Id) ->
    case create_log_attr_table(Id) of
	{ok, LogAttrName} ->
	    LogMgr = #log_factory{id=Id, objref=self()},
	    F = fun () -> mnesia:write(oe_tlsbf, LogMgr, write)	end,
	    {atomic, ok} = mnesia:transaction(F),
	    {ok, #state{id=Id, table_name=LogAttrName}};
	{error, Reason} ->
	    {stop, Reason}
    end.

get_log_mgr_id(_) ->
    case 'DsLogAdmin_Common':create_table(oe_tlsbf,
					  record_info(fields, log_factory),
					  log_factory) of
	{ok, {existing_table, oe_tlsbf}} ->
	    %% First non-existing log manager number
	    %% TODO fix this
	    {ok, next_log_mgr_id()};
	{ok, {new_table, oe_tlsbf}} ->
	    {ok, 1};
	{error, Reason} ->
	    {error, Reason}
    end.
