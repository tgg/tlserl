-module('DsLogAdmin_Log_impl').

-export([destroy/2]).
-export([my_factory/2, id/2, get_log_qos/2]).
-export([set_log_qos/3, get_max_record_life/2, set_max_record_life/3]).
-export([get_max_size/2, set_max_size/3, get_current_size/2]).
-export([get_n_records/2, get_log_full_action/2, set_log_full_action/3]).
-export([get_administrative_state/2, set_administrative_state/3, get_forwarding_state/2]).
-export([set_forwarding_state/3, get_operational_state/2, get_interval/2]).
-export([set_interval/3, get_availability_status/2, get_capacity_alarm_thresholds/2]).
-export([set_capacity_alarm_thresholds/3, get_week_mask/2, set_week_mask/3]).
-export(['query'/4, retrieve/4, match/4]).
-export([delete_records/4, delete_records_by_id/3, write_records/3]).
-export([write_recordlist/3, set_record_attribute/4, set_records_attribute/5]).
-export([get_record_attribute/3, copy/2, copy_with_id/3]).
-export([flush/2]).

%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%----------------------------------------------------------------------
%% Include Files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("TimeBase.hrl").
-include("DsLogAdmin.hrl").

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(ABSOLUTE_TIME_DIFF, 49947926400).
-define(FULL_AND_HALT,
	#state{full_action=1,
	       availability_status=#'DsLogAdmin_AvailabilityStatus'{log_full=true}}).
-define(OFF_DUTY,
	#state{availability_status=#'DsLogAdmin_AvailabilityStatus'{off_duty=true}}).
-define(LOCKED, #state{administrative_state=locked}).
-define(DISABLED, #state{operational_state=disabled}).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {factory,
		id,
		qos = ['DsLogAdmin':'QoSNone'()],
		max_record_life = 0,
		max_size = 0,
		full_action,
	        administrative_state = unlocked,
		forward_state = off,
		operational_state = enabled,
		interval = #'TimeBase_IntervalT'{lower_bound=0, upper_bound=0},
		availability_status = #'DsLogAdmin_AvailabilityStatus'{off_duty=false, log_full=false},
		capacity_alarm_thresholds = [],
		week_mask = []}).

%%======================================================================
%% API Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : destroy/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = ok
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
destroy(OE_This, State) ->
	%% TODO does not work
	{reply, corba:dispose(OE_This), State}.

%%----------------------------------------------------------------------
%% Function   : my_factory/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = Object_Ref()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
my_factory(_OE_This, State) ->
	{reply, State#state.factory, State}.

%%----------------------------------------------------------------------
%% Function   : id/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
id(_OE_This, State) ->
	{reply, State#state.id, State}.

%%----------------------------------------------------------------------
%% Function   : get_log_qos/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = unsigned_Short()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_log_qos(_OE_This, State) ->
	{reply, State#state.qos, State}.

%%----------------------------------------------------------------------
%% Function   : set_log_qos/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Qos = [ QosElem ]
%%              QosElem = unsigned_Short()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::UnsupportedQoS
%% Description: 
%%----------------------------------------------------------------------
set_log_qos(_OE_This, State, Qos) when is_list(Qos) ->
	check_qos_list(Qos),
	{reply, ok, State#state{qos=Qos}};
set_log_qos(_OE_This, _State, _Qos) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : get_max_record_life/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_max_record_life(_OE_This, State) ->
	{reply, State#state.max_record_life, State}.

%%----------------------------------------------------------------------
%% Function   : set_max_record_life/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Life = unsigned_Long()
%% Returns    : ReturnValue = ok
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
set_max_record_life(_OE_This, State, Life) when is_integer(Life) andalso Life >= 0 ->
	{reply, ok, State#state{max_record_life=Life}};
set_max_record_life(_OE_This, _State, _Life) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : get_max_size/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_max_size(_OE_This, State) ->
	{reply, State#state.max_size, State}.

%%----------------------------------------------------------------------
%% Function   : set_max_size/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Size = unsigned_Long_Long()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::InvalidParam
%% Description: 
%%----------------------------------------------------------------------
set_max_size(_OE_This, State, Size) when is_integer(Size) andalso Size >= 0 ->
	{reply, ok, State#state{max_size=Size}};
set_max_size(_OE_This, _State, _Size) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : get_current_size/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_current_size(_OE_This, State) ->
    Name = name(State),
    {reply, mnesia:table_info(Name, memory), State}.

%%----------------------------------------------------------------------
%% Function   : get_n_records/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_n_records(_OE_This, State) ->
    Name = name(State),
    {reply, mnesia:table_info(Name, size), State}.

%%----------------------------------------------------------------------
%% Function   : get_log_full_action/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Short()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_log_full_action(_OE_This, State) ->
	{reply, State#state.full_action, State}.

%%----------------------------------------------------------------------
%% Function   : set_log_full_action/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Action = unsigned_Short()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::InvalidLogFullAction
%% Description: 
%%----------------------------------------------------------------------
set_log_full_action(_OE_This, State, Full_action) ->
	'DsLogAdmin_Common':check_full_action(Full_action),
	{reply, ok, State#state{full_action=Full_action}}.

%%----------------------------------------------------------------------
%% Function   : get_administrative_state/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = AdministrativeState
%%              AdministrativeState = 'locked' | 'unlocked' 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_administrative_state(_OE_This, State) ->
	{reply, State#state.administrative_state, State}.

%%----------------------------------------------------------------------
%% Function   : set_administrative_state/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              State = AdministrativeState
%%              AdministrativeState = 'locked' | 'unlocked' 
%% Returns    : ReturnValue = ok
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
set_administrative_state(_OE_This, State, AdminState)
  when AdminState =:= locked; AdminState =:= unlocked ->
	{reply, ok, State#state{administrative_state=AdminState}};
set_administrative_state(_OE_This, _State, _AdminState) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : get_forwarding_state/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = ForwardingState
%%              ForwardingState = 'on' | 'off' 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_forwarding_state(_OE_This, State) ->
	{reply, State#state.forward_state, State}.

%%----------------------------------------------------------------------
%% Function   : set_forwarding_state/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              State = ForwardingState
%%              ForwardingState = 'on' | 'off' 
%% Returns    : ReturnValue = ok
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
set_forwarding_state(_OE_This, State, ForwardingState)
  when ForwardingState =:= on; ForwardingState =:= off ->
	{reply, ok, State#state{forward_state=ForwardingState}};
set_forwarding_state(_OE_This, _State, _ForwardingState) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : get_operational_state/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = OperationalState
%%              OperationalState = 'disabled' | 'enabled' 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_operational_state(_OE_This, State) ->
	{reply, State#state.operational_state, State}.

%%----------------------------------------------------------------------
%% Function   : get_interval/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = #'DsLogAdmin_TimeInterval'{start,stop}
%%              start = unsigned_Long_Long()
%%              stop = unsigned_Long_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_interval(_OE_This, State) ->
	{reply, State#state.interval, State}.

%%----------------------------------------------------------------------
%% Function   : set_interval/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Interval = #'DsLogAdmin_TimeInterval'{start,stop}
%%              start = unsigned_Long_Long()
%%              stop = unsigned_Long_Long()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::InvalidTime
%%              DsLogAdmin::InvalidTimeInterval
%% Description: 
%%----------------------------------------------------------------------
set_interval(_OE_This, State, Interval) ->
	{reply, ok, State#state{interval=Interval}}.

%%----------------------------------------------------------------------
%% Function   : get_availability_status/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = #'DsLogAdmin_AvailabilityStatus'{off_duty,log_full}
%%              off_duty = boolean()
%%              log_full = boolean()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_availability_status(_OE_This, State) ->
	{reply, State#state.availability_status, State}.

%%----------------------------------------------------------------------
%% Function   : get_capacity_alarm_thresholds/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = unsigned_Short()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_capacity_alarm_thresholds(_OE_This, State) ->
	{reply, State#state.capacity_alarm_thresholds, State}.

%%----------------------------------------------------------------------
%% Function   : set_capacity_alarm_thresholds/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Threshs = [ ThreshsElem ]
%%              ThreshsElem = unsigned_Short()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::InvalidThreshold
%% Description: 
%%----------------------------------------------------------------------
set_capacity_alarm_thresholds(_OE_This, State, Threshs) ->
	{reply, ok, State#state{availability_status=Threshs}}.

%%----------------------------------------------------------------------
%% Function   : get_week_mask/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = #'DsLogAdmin_WeekMaskItem'{days,intervals}
%%              days = unsigned_Short()
%%              intervals = [ intervalsElem ]
%%              intervalsElem = #'DsLogAdmin_Time24Interval'{start,stop}
%%              start = #'DsLogAdmin_Time24'{hour,minute}
%%              hour = unsigned_Short()
%%              minute = unsigned_Short()
%%              stop = #'DsLogAdmin_Time24'{hour,minute}
%%              hour = unsigned_Short()
%%              minute = unsigned_Short()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
get_week_mask(_OE_This, State) ->
	{reply, State#state.week_mask, State}.

%%----------------------------------------------------------------------
%% Function   : set_week_mask/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Masks = [ MasksElem ]
%%              MasksElem = #'DsLogAdmin_WeekMaskItem'{days,intervals}
%%              days = unsigned_Short()
%%              intervals = [ intervalsElem ]
%%              intervalsElem = #'DsLogAdmin_Time24Interval'{start,stop}
%%              start = #'DsLogAdmin_Time24'{hour,minute}
%%              hour = unsigned_Short()
%%              minute = unsigned_Short()
%%              stop = #'DsLogAdmin_Time24'{hour,minute}
%%              hour = unsigned_Short()
%%              minute = unsigned_Short()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::InvalidTime
%%              DsLogAdmin::InvalidTimeInterval
%%              DsLogAdmin::InvalidMask
%% Description: 
%%----------------------------------------------------------------------
set_week_mask(_OE_This, State, Masks) ->
	{reply, ok, State#state{week_mask=Masks}}.

%%----------------------------------------------------------------------
%% Function   : 'query'/4
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Grammar = String()
%%              C = String()
%% Returns    : ReturnValue = {OE_Reply, I}
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = #'DsLogAdmin_LogRecord'{id,time,attr_list,info}
%%              id = unsigned_Long_Long()
%%              time = unsigned_Long_Long()
%%              attr_list = [ attr_listElem ]
%%              attr_listElem = #'DsLogAdmin_NVPair'{name,value}
%%              name = String()
%%              value = any()
%%              info = any()
%%              I = Object_Ref()
%% Raises     : DsLogAdmin::InvalidGrammar
%%              DsLogAdmin::InvalidConstraint
%% Description: 
%%----------------------------------------------------------------------
'query'(_OE_This, State, Grammar, Constraint) ->
	OE_Reply = filter(State, Grammar, Constraint, fun match_record/2),
	io:format("query returning ~p~n", [OE_Reply]),
	{reply, {OE_Reply, corba:create_nil_objref()}, State}.

%%----------------------------------------------------------------------
%% Function   : retrieve/4
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              From_time = unsigned_Long_Long()
%%              How_many = long()
%% Returns    : ReturnValue = {OE_Reply, I}
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = #'DsLogAdmin_LogRecord'{id,time,attr_list,info}
%%              id = unsigned_Long_Long()
%%              time = unsigned_Long_Long()
%%              attr_list = [ attr_listElem ]
%%              attr_listElem = #'DsLogAdmin_NVPair'{name,value}
%%              name = String()
%%              value = any()
%%              info = any()
%%              I = Object_Ref()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
retrieve(_OE_This, State, From_time, How_many)
  when is_integer(From_time) andalso From_time >= 0
       andalso is_integer(How_many) ->
	if
	    How_many < 0 ->
		M = do(qlc:q([L || L <- table(State), L#'DsLogAdmin_LogRecord'.time  < From_time]));
	    true ->
		M = do(qlc:q([L || L <- table(State), L#'DsLogAdmin_LogRecord'.time >= From_time]))
	end,
	{reply, {lists:sublist(M, abs(How_many)), corba:create_nil_objref()}, State};
retrieve(_OE_This, _State, _From_time, _How_many) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : match/4
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Grammar = String()
%%              Constraint = String()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long()
%% Raises     : DsLogAdmin::InvalidGrammar
%%              DsLogAdmin::InvalidConstraint
%% Description: 
%%----------------------------------------------------------------------
match(_OE_This, State, Grammar, Constraint) ->
	OE_Reply = length(filter(State, Grammar, Constraint, fun match_record/2)),
	{reply, OE_Reply, State}.

%%----------------------------------------------------------------------
%% Function   : delete_records/4
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Grammar = String()
%%              C = String()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long()
%% Raises     : DsLogAdmin::InvalidGrammar
%%              DsLogAdmin::InvalidConstraint
%% Description: 
%%----------------------------------------------------------------------
delete_records(_OE_This, State, Grammar, Constraint) ->
    F = fun() ->
		Ids = filter(State, Grammar, Constraint,
			     fun match_record/2,
			     fun(R) -> R#'DsLogAdmin_LogRecord'.id end),
		delete_records_by_id(_OE_This, State, Ids)
	end,
    {atomic, Reply} = mnesia:transaction(F),
    Reply.

%%----------------------------------------------------------------------
%% Function   : delete_records_by_id/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Ids = [ IdsElem ]
%%              IdsElem = unsigned_Long_Long()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
delete_records_by_id(_OE_This, State, Ids) ->
    Name = name(State),
    Oids = [{Name, Id} || Id <- Ids],
    {reply, delete_records(Name, Oids, 0), State}.

%%----------------------------------------------------------------------
%% Function   : write_records/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Records = [ RecordsElem ]
%%              RecordsElem = any()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::LogFull
%%              DsLogAdmin::LogOffDuty
%%              DsLogAdmin::LogLocked
%%              DsLogAdmin::LogDisabled
%% Description: 
%%----------------------------------------------------------------------
write_records(_OE_This, State, Records) ->
    NewState = add_records(State, anys_to_logrecords(Records)),
    {reply, ok, NewState}.

%%----------------------------------------------------------------------
%% Function   : write_recordlist/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              List = [ ListElem ]
%%              ListElem = #'DsLogAdmin_LogRecord'{id,time,attr_list,info}
%%              id = unsigned_Long_Long()
%%              time = unsigned_Long_Long()
%%              attr_list = [ attr_listElem ]
%%              attr_listElem = #'DsLogAdmin_NVPair'{name,value}
%%              name = String()
%%              value = any()
%%              info = any()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::LogFull
%%              DsLogAdmin::LogOffDuty
%%              DsLogAdmin::LogLocked
%%              DsLogAdmin::LogDisabled
%% Description: 
%%----------------------------------------------------------------------
write_recordlist(_OE_This, State, List) ->
    NewState = add_records(State, List),
    {reply, ok, NewState}.

%%----------------------------------------------------------------------
%% Function   : set_record_attribute/4
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Id = unsigned_Long_Long()
%%              Attr_list = [ Attr_listElem ]
%%              Attr_listElem = #'DsLogAdmin_NVPair'{name,value}
%%              name = String()
%%              value = any()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::InvalidRecordId
%%              DsLogAdmin::InvalidAttribute
%% Description: 
%%----------------------------------------------------------------------
set_record_attribute(_OE_This, State, Id, Attr_list) ->
	{reply, ok, State}.

%%----------------------------------------------------------------------
%% Function   : set_records_attribute/5
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Grammar = String()
%%              C = String()
%%              Attr_list = [ Attr_listElem ]
%%              Attr_listElem = #'DsLogAdmin_NVPair'{name,value}
%%              name = String()
%%              value = any()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = unsigned_Long()
%% Raises     : DsLogAdmin::InvalidGrammar
%%              DsLogAdmin::InvalidConstraint
%%              DsLogAdmin::InvalidAttribute
%% Description: 
%%----------------------------------------------------------------------
set_records_attribute(_OE_This, State, Grammar, C, Attr_list) ->
	{reply, 0, State}.

%%----------------------------------------------------------------------
%% Function   : get_record_attribute/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Id = unsigned_Long_Long()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = #'DsLogAdmin_NVPair'{name,value}
%%              name = String()
%%              value = any()
%% Raises     : DsLogAdmin::InvalidRecordId
%% Description: 
%%----------------------------------------------------------------------
get_record_attribute(_OE_This, State, Id) ->
	%{reply, OE_Reply, State}.
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : copy/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = {OE_Reply, Id}
%%              OE_Reply = Object_Ref()
%%              Id = unsigned_Long()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
copy(_OE_This, State) ->
	%{reply, {OE_Reply, Id}, State}.
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : copy_with_id/3
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Id = unsigned_Long()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = Object_Ref()
%% Raises     : DsLogAdmin::LogIdAlreadyExists
%% Description: 
%%----------------------------------------------------------------------
copy_with_id(_OE_This, State, Id) ->
	%{reply, OE_Reply, State}.
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : flush/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = ok
%% Raises     : DsLogAdmin::UnsupportedQoS
%% Description: 
%%----------------------------------------------------------------------
flush(_OE_This, State) ->
	{reply, ok, State}.

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
init([Factory, Id, Full_action, Max_size]) ->
    State = #state{factory=Factory,
		   id=Id,
		   full_action=Full_action,
		   max_size=Max_size},
    {atomic, ok} = mnesia:create_table(name(State),
				       [{attributes, record_info(fields, 'DsLogAdmin_LogRecord')},
					{record_name, 'DsLogAdmin_LogRecord'}]),
    {ok, State}.


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

check_qos_list([]) ->
	ok;
check_qos_list([H|T]) when is_integer(H) ->
	None = 'DsLogAdmin':'QoSNone'(),
	Flush = 'DsLogAdmin':'QoSFlush'(),
	Reliability = 'DsLogAdmin':'QoSReliability'(),
	case H of
	    None        -> check_qos_list(T);
	    Flush       -> check_qos_list(T);
	    Reliability -> check_qos_list(T);
	    _           -> corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	end;
check_qos_list(_L) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%% Copied verbatim from CosTime_TimeService_impl:create_universal_time().
get_time() ->
	{MS,S,US} = erlang:now(),
	Secs = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time({MS,S,US})),
	(Secs-?ABSOLUTE_TIME_DIFF)*10000000 + US*10.

%% Inspired from cosNotification_Filter.erl
new_evaluator(Grammar, Query) ->
	if
	    Grammar =:= "EXTENDED_TCL" ->
		try cosNotification_Scanner:scan(Query) of
		    {ok, Tokens}  ->
			try cosNotification_Grammar:parse(Tokens) of
			    {ok, Evaluator} ->
				{ok, Evaluator};
			    {error, _Why} ->
				io:format("Error when parsing tokens: ~p~n", [_Why]),
				{error, bad_constraint}
			catch
			    exit:_Why ->
				io:format("Exit when parsing tokens: ~p~n", [_Why]),
				{error, bad_constraint}
			end;
		    {error, _Why} ->
			io:format("Error when scanning query: ~p~n", [_Why]),
			{error, bad_constraint}
		catch
		    error:_Why ->
			io:format("Exit when scanning query: ~p~n", [_Why]),
			{error, bad_constraint}
		end;
	    true ->
		io:format("Invalid grammar: ~p~n", [Grammar]),
		{error, unknown_grammar}
	end.

match_record(Evaluator, Record) ->
	cosNotification_Filter:eval(Evaluator, Record).

do_evaluate(Record, {Predicate, Filter, Evaluator, Acc} = Args) ->
    case Predicate(Evaluator, Record) of
	true -> {Predicate, Filter, Evaluator, [Filter(Record)|Acc]};
	_    -> Args
    end.

identity(Something) ->
    Something.

filter(State, Grammar, Constraint, Predicate) ->
    filter(State, Grammar, Constraint, Predicate, fun identity/1).

filter(State, Grammar, Constraint, Predicate, Filter) ->
    case new_evaluator(Grammar, Constraint) of
	{ok, Evaluator} ->
	    F = fun () ->
			mnesia:foldl(fun do_evaluate/2, {Predicate, Filter, Evaluator, []}, name(State))
		end,
	    {atomic, {_, _, _, Val}} = mnesia:transaction(F),
	    Val;
	{error, unknown_grammar} ->
	    corba:raise(#'DsLogAdmin_InvalidGrammar'{});
	{error, bad_constraint} ->
	    corba:raise(#'DsLogAdmin_InvalidConstraint'{})
    end.

anys_to_logrecords(Anys) when is_list(Anys) ->
    anys_to_logrecords(Anys, []);
anys_to_logrecords(_Anys) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

anys_to_logrecords([Any | Anys], Acc) when is_record(Any, any) ->
    % Other elements will be set later on.
    Record = #'DsLogAdmin_LogRecord'{attr_list=[], info=Any},
    anys_to_logrecords(Anys, [Record | Acc]);
anys_to_logrecords([], Acc) ->
    lists:reverse(Acc);
anys_to_logrecords(_Anys, _Acc) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

next_id(Record, Max) ->
    max(Record#'DsLogAdmin_LogRecord'.id, Max).

next_id(State) ->
    F = fun () ->
		1 + mnesia:foldl(fun next_id/2, 0, name(State))
	end,
    {atomic, Id} = mnesia:transaction(F),
    Id.

name(State) ->
    list_to_atom("tlsb_" ++ erlang:integer_to_list(State#state.id, 10)).

do(Q) ->
    F = fun () ->
		qlc:e(Q)
	end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

table(State) ->
    mnesia:table(name(State)).

% TODO: error handling. Return ok or error. Do not raise CORBA exceptions here?
add_records(?FULL_AND_HALT, _Records) ->
    corba:raise(#'DsLogAdmin_LogFull'{n_records_written=0});
add_records(?OFF_DUTY, _Records) ->
    corba:raise(#'DsLogAdmin_LogOffDuty'{});
add_records(?LOCKED, _Records) ->
    corba:raise(#'DsLogAdmin_LogLocked'{});
add_records(?DISABLED, _Records) ->
    corba:raise(#'DsLogAdmin_LogDisabled'{});
add_records(State, [Record | Records])
  when is_record(Record, 'DsLogAdmin_LogRecord') ->
    Id = next_id(State),
    NewRecord = Record#'DsLogAdmin_LogRecord'{id=Id, time=get_time()},
    Table = name(State),
    F = fun () ->
		mnesia:write(Table, NewRecord, write)
	end,
    {_, Val} = mnesia:transaction(F),
    add_records(State, Records);
add_records(State, []) ->
    State;
add_records(_State, _Records) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

delete_records(Name, [Oid | Oids], Count) ->
    F = fun() ->
		Match = mnesia:read(Oid),
		mnesia:delete(Oid),
		length(Match)
	end,
    {atomic, N} = mnesia:transaction(F),
    delete_records(Name, Oids, N + Count);
delete_records(_Name, [], Count) ->
    Count.
