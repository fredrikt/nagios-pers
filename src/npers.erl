%%%-------------------------------------------------------------------
%%% File    : npers.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip : Main module.
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(npers).

-export([
	 start/0
	]).

start() ->
    Options = [{interval, 300}],
    npers_sup:start_link(Options),

    {ok, Checks} = npers_checks:get_all(Options),

    gen_server:call(npers_spawner, {set_checks, Checks}).

    
	       
