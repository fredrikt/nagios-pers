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

get_all(Options) ->
    Filename = proplists:get_value(erl_checks_filename, Options),
    {ok, [CheckList]} = file:consult(Filename),
    {ok, CheckList}.
