
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
%% File    : DsLogAdmin_Common.erl
%% Purpose : Contains common code used by both the log and factory
%%           implementations.
%%----------------------------------------------------------------------

-module('DsLogAdmin_Common').

%%----------------------------------------------------------------------
%% Include Files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").

-include("DsLogAdmin.hrl").

%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([check_max_size/1, check_full_action/1, create_table/3, do/1,
	 log_table_name/2, log_attr_table_name/1, lookup_log_attributes/2]).


check_max_size(Max_size) when is_integer(Max_size) andalso Max_size >= 0 ->
    ok;
check_max_size(_Max_size) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

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

create_table(Name, RecordInfo, RecordName) ->
    AllTables = mnesia:system_info(tables),
    case lists:member(Name, AllTables) of
	true ->
	    {ok, {existing_table, Name}};
	false ->
	    case mnesia:create_table(Name,
				     [{attributes, RecordInfo},
				      {record_name, RecordName}]) of
		{atomic, ok} ->
		    {ok, {new_table, Name}};
		{aborted, Reason} ->
		    {error, Reason}
	    end
    end.

do(Q) ->
    F = fun () -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

log_table_name(Fid, Id) ->
    list_to_atom("oe_tlsb_" ++ integer_to_list(Fid) ++ "_" ++ integer_to_list(Id)).

log_attr_table_name(Fid) ->
    list_to_atom("oe_tlsbf_" ++ integer_to_list(Fid) ++ "_log_attr").

lookup_log_attributes(Fid, Id) ->
    TableName = log_attr_table_name(Fid),
    F = fun () -> mnesia:read(TableName, Id) end,
    {atomic, Val} = mnesia:transaction(F),
    io:format("Searched in ~p for Id: ~p, found: ~p~n", [TableName, Id, Val]),
    Val.
