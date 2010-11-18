%%%-------------------------------------------------------------------
%%% File    : npers_checks.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip : Check configuration handling.
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(npers_checks).

-export([
	 get_all/1
	]).

get_all(_Options) ->
    %% Mock
    MockPing = {nagios_check, "/usr/lib/nagios/plugins/check_ping",
		["-H", "192.168.5.1", "-w", "100.0,20%", "-c", "500.0,60%", "-p", "5"]
	       },
    L = [MockPing,MockPing,MockPing,MockPing,MockPing,MockPing,MockPing,MockPing],
    {ok, L}.

