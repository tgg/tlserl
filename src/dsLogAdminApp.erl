-module(dsLogAdminApp).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, Args) ->
    ok.

stop(_State) ->
    ok.

