%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: DsLogAdmin_IntervalsOfDay
%% Source: /home/tgg/src/hg/tlserl/src/DsLogAdmin.idl
%% IC vsn: 4.2.25
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('DsLogAdmin_IntervalsOfDay').
-ic_compiled("4_2_25").


-include("DsLogAdmin.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_sequence,
            {tk_struct,"IDL:omg.org/DsLogAdmin/Time24Interval:1.0",
                "Time24Interval",
                [{"start",
                  {tk_struct,"IDL:omg.org/DsLogAdmin/Time24:1.0","Time24",
                      [{"hour",tk_ushort},{"minute",tk_ushort}]}},
                 {"stop",
                  {tk_struct,"IDL:omg.org/DsLogAdmin/Time24:1.0","Time24",
                      [{"hour",tk_ushort},{"minute",tk_ushort}]}}]},
            0}.

%% returns id
id() -> "IDL:omg.org/DsLogAdmin/IntervalsOfDay:1.0".

%% returns name
name() -> "DsLogAdmin_IntervalsOfDay".



