%%%-------------------------------------------------------------------
%%% File    : npers.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip : Main module.
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(npers).

-export([
	 start/0,
	 start/1,
	 start_test/0,
	 send_result/5
	]).

%% interval            : the number of seconds you want all checks to be completed in
%% erl_checks_filename : path to file with check definitions - see README for details
%% send_result_cmd     : path to script that will send results to your Nagios master server
-define(OPTIONS, [{interval, 600},
		  {erl_checks_filename, "erl.cfg"},
		  {send_result_cmd, "/local/nagios/system-checkout/nagios/scripts/checks/send_result"}
		 ]
       ).

start() ->
    Options = ?OPTIONS,
    start(Options).

%% for testing - {test_mode, {true, undefined}} means that no check commands will be executed
start_test() ->
    Options = [{interval, 30},
	       {erl_checks_filename, "erl.cfg"},
	       {send_result_cmd, "/local/nagios/system-checkout/nagios/scripts/checks/send_result"},
	       {test_mode, {true, undefined}}
	      ],

    start(Options).

start(Options) ->
    {ok, Checks} = npers_checks:get_all(Options),

    application:start(sasl),

    npers_sup:start_link(Options),

    gen_server:call(npers_spawner, {set_checks, Checks}).

    
	       
send_result(HostName, CheckName, ExitStatus, Output, Options) ->
    npers_nsca:got_result(HostName, CheckName, ExitStatus, Output, Options),
    %%io:format("S:~p  ~s~n", [ExitStatus, Output]),
    ok.
