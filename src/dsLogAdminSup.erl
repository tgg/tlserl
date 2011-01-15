-module(dsLogAdminSup).
-behaviour(supervisor).

-export([start_link/1, init/1, create_link/3]).

start_link(_) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 3, 10},
	  [{"dsLogAdminChild",
	   {?MODULE, create_link, []},
	   transient, 100000, worker,
	   ['DsLogAdmin_BasicLogFactory',
	    'DsLogAdmin_Factory_impl']}]}}.

create_link(Module, Env, ArgList) ->
    Module:oe_create_link(Env, ArgList).
