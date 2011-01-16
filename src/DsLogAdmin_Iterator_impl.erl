%%----------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Thomas Girard <thomas.g.girard@free.fr> 2011. All Rights Reserved.
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
%% File    : DsLogAdmin_Iterator.erl
%% Purpose : Implementation of DsLogAdmin:LogIterator.
%%----------------------------------------------------------------------

-module('DsLogAdmin_Iterator_impl').

-export([get/4, destroy/2]).

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

-include("DsLogAdmin.hrl").

%%======================================================================
%% API Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : get/4
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%%              Position = unsigned_Long()
%%              How_many = unsigned_Long()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = [ OE_ReplyElem ]
%%              OE_ReplyElem = #'DsLogAdmin_LogRecord'{id,time,attr_list,info}
%%              id = unsigned_Long_Long()
%%              time = unsigned_Long_Long()
%%              attr_list = [ attr_listElem ]
%%              attr_listElem = #'DsLogAdmin_NVPair'{name,value}
%%              name = String()
%%              value = any()
%%              info = any()
%% Raises     : DsLogAdmin::InvalidParam
%% Description: 
%%----------------------------------------------------------------------
get(_OE_This, State, Position, How_many)
  when is_integer(Position)
       andalso Position >= element(1, State)
       andalso Position =< length(element(0, State)) ->
	{_, List} = State,
	{reply, lists:sublist(List, Position, How_many), {List, Position}};
get(_OE_This, _State, Position, _How_many)
  when is_integer(Position) ->
	corba:raise(#'DsLogAdmin_InvalidParam'{});
get(_OE_This, _State, _Position, _How_many) ->
	corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
%%----------------------------------------------------------------------
%% Function   : destroy/2
%% Arguments  : OE_This - #objref{} (i.e., self())
%%              State - term()
%% Returns    : ReturnValue = ok
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
destroy(_OE_This, State) ->
	{reply, ok, State}.

%%======================================================================
%% Internal Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Arguments  : Records = term()
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Raises     : -
%% Description: Initiates the server
%%----------------------------------------------------------------------
init(Records) ->
	{ok, {Records, 0}}.


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


