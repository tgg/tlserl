%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: DsLogAdmin_NVPair
%% Source: /home/tgg/src/hg/tlserl/src/DsLogAdmin.idl
%% IC vsn: 4.2.25
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('DsLogAdmin_NVPair').
-ic_compiled("4_2_25").


-include("DsLogAdmin.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/DsLogAdmin/NVPair:1.0","NVPair",
                   [{"name",{tk_string,0}},{"value",tk_any}]}.

%% returns id
id() -> "IDL:omg.org/DsLogAdmin/NVPair:1.0".

%% returns name
name() -> "DsLogAdmin_NVPair".


