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
	 send_result/5
	]).

start() ->
    Options = [{interval, 300},
	       {erl_checks_filename, "erl.cfg"}
	      ],

    {ok, Checks} = npers_checks:get_all(Options),

    application:start(sasl),

    npers_sup:start_link(Options),

    gen_server:call(npers_spawner, {set_checks, Checks}).

    
	       
send_result(HostName, CheckName, ExitStatus, Output, Options) ->
    npers_nsca:got_result(HostName, CheckName, ExitStatus, Output, Options),
    %%io:format("S:~p  ~s~n", [ExitStatus, Output]),
    ok.
