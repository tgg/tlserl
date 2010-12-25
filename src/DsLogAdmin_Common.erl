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
