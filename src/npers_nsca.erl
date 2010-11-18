%%%-------------------------------------------------------------------
%%% File    : npers_nsca.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip : Report check results to master Nagios server.
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(npers_nsca).

-export([
	 got_result/5
	]).

got_result(HostName, CheckName, ExitStatus, Output, Options) ->
    ok.
