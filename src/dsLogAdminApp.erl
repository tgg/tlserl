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
%% File    : dsLogAdminApp.erl
%% Purpose : Contains application specific callbacks.
%%----------------------------------------------------------------------

-module(dsLogAdminApp).
-behaviour(application).

%%----------------------------------------------------------------------
%% Includes
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").

%%----------------------------------------------------------------------
%% Exports
%%----------------------------------------------------------------------
%% API external
-export([install/0, start/0, stop/0, uninstall/0, start_logmgr/0]).

%% application callbacks
-export([start/2, stop/1]).

%%----------------------------------------------------------------------
%% Function   : install/0
%% Arguments  : none
%% Returns    : ReturnValue = ok
%% Description: Install necessary data in the IFR database.
%%----------------------------------------------------------------------
install() ->
    try_install([{'oe_TimeBase', [allready_registered]},
		 {'oe_DsLogAdmin', []}],
	       []).

try_install([], _) ->
    ok;
try_install([{M, IgnoreList}|Tail], Acc) ->
    try M:'oe_register'() of
	ok -> try_install(Tail, [M|Acc])
    catch
	throw:Reason when is_tuple(Reason) andalso size(Reason) >= 1 ->
	    Tag = element(1, Reason),
	    case lists:member(Tag, IgnoreList) of
		true ->
		    try_install(Tail, Acc);
		false ->
		    io:format("'~p':'oe_register' failed: ~p~n", [M, Reason]),
		    uninstall(Acc, Reason)
	    end;
	throw:Reason ->
	    io:format("'~p':'oe_register' failed: ~p~n", [M, Reason]),
	    uninstall(Acc, Reason);
	exit:Reason when is_tuple(Reason) andalso size(Reason) >= 1 ->
	    Tag = element(1, Reason),
	    case lists:member(Tag, IgnoreList) of
		true ->
		    try_install(Tail, Acc);
		false ->
		    io:format("'~p':'oe_register' failed: ~p~n", [M, Reason]),
		    uninstall(Acc, Reason)
	    end;
	exit:Reason ->
	    io:format("'~p':'oe_register' failed: ~p~n", [M, Reason]),
	    uninstall(Acc, Reason)
    end.

%%----------------------------------------------------------------------
%% Function   : uninstall/0
%% Arguments  : none
%% Returns    : ReturnValue = ok | 
%% Description: Uninstall data from the IFR database
%%----------------------------------------------------------------------
uninstall() ->
    %% We don't uninstall 'oe_TimeBase', because we don't know here if we
    %% installed it or not.
    uninstall(['oe_DsLogAdmin'], ok).

uninstall([], Return) ->
    Return;
uninstall([Module|Tail], Return) ->
    try Module:'oe_unregister'()
    catch
	throw:Reason ->
	    io:format("'~p':'oe_unregister' failed: ~p~n", [Module, Reason]);
	exit:Reason ->
	    io:format("'~p':'oe_unregister' failed: ~p~n", [Module, Reason])
    end,
    uninstall(Tail, Return).

%%----------------------------------------------------------------------
%% Function   : start/0
%% Arguments  : none
%% Returns    : ReturnValue = ok
%% Description: Shortcut to start the application.
%%----------------------------------------------------------------------
start() ->
    application:start(dsLogAdmin).

%%----------------------------------------------------------------------
%% Function   : stop/0
%% Arguments  : none
%% Returns    : ReturnValue = ok
%% Description: Shortcut to stop the application.
%%----------------------------------------------------------------------
stop() ->
    application:stop(dsLogAdmin).

start_logmgr() ->
    Spec = ['DsLogAdmin_BasicLogFactory', nothing,
	    [{sup_child, true},
	     {regname, {local, oe_dsBasicLogFactory}}]],
    case supervisor:start_child(dsLogAdminSup, Spec) of
	{ok, Pid, Obj} when is_pid(Pid) ->
	    Obj;
	Other ->
	    io:format("Error when starting BasicLogFactory: ~p~n", [Other]),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : start/2
%% Arguments  : _StartType ignored
%%              StartArgs arguments passed to mod in .app file
%% Returns    : ReturnValue = {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% Description: This function is called when the application is started
%% with application:start/1,2. It starts our supervision process.
%%----------------------------------------------------------------------
start(_StartType, StartArgs) ->
    dsLogAdminSup:start_link(StartArgs).

stop(_State) ->
    ok.
