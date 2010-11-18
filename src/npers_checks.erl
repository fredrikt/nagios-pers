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
    %% Shuffle result to achieve at least a primitive form of what
    %% Nagios documentation calls 'interleaving'. Meaning don't hit
    %% all services on one host at the exact same time.
    {ok, shuffle(CheckList)}.

%% Fisher-Yates shuffle from
%% http://en.literateprograms.org/Fisher-Yates_shuffle_%28Erlang%29
shuffle(L) ->
    shuffle(L, []).

shuffle([], Acc) ->
     Acc;
shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).
