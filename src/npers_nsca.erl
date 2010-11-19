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

got_result(HostName, CheckName, ExitStatus, Output, Options)
  when is_list(HostName), is_list(CheckName), is_integer(ExitStatus), is_list(Output), is_list(Options) ->
    spawn(fun() ->
		  start_nsca(HostName, CheckName, ExitStatus, Output, Options)
	  end
	 ),
    ok.

start_nsca(HostName, CheckName, ExitStatus, Output, Options) ->
    Cmd = proplists:get_value(send_result_cmd, Options,
			      "/local/nagios/system-checkout/nagios/scripts/checks/send_result"),

    StatusStr = status2str(ExitStatus),

    Args = [HostName,
	    CheckName,
	    StatusStr,
	    Output
	   ],

    Port =
	erlang:open_port({spawn_executable, Cmd},
			 [{args, Args},
			  out
			 ]),

    %%io:format("~p : Spawned port ~p - ~s ~p~n", [self(), Port, Cmd, Args]),

    timer:sleep(1000),

    erlang:port_close(Port),

    %%io:format("Messages : ~p~n", [erlang:process_info(self(), messages)]),

    ok.

status2str(0) -> "OK";
status2str(1) -> "WARNING";
status2str(2) -> "CRITICAL";
status2str(_) -> "UNKNOWN".
