%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: DsLogAdmin_LogFull
%% Source: /home/tgg/src/hg/tlserl/src/DsLogAdmin.idl
%% IC vsn: 4.2.25
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('DsLogAdmin_LogFull').
-ic_compiled("4_2_25").


-include("DsLogAdmin.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/DsLogAdmin/LogFull:1.0","LogFull",
                   [{"n_records_written",tk_short}]}.

%% returns id
id() -> "IDL:omg.org/DsLogAdmin/LogFull:1.0".

%% returns name
name() -> "DsLogAdmin_LogFull".



