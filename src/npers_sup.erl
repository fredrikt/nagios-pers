%%%-------------------------------------------------------------------
%%% File    : npers_sup.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip : Main supervisor. 
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(npers_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Options) when is_list(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(Options) when is_list(Options) ->
    Spawner = {'npers_spawner',
	       {'npers_spawner', start_link, [Options]},
	       permanent,
	       2000,
	       worker,
	       ['npers_spawner']
	      },

    {ok, {{one_for_all, 0, 1}, [Spawner]}}.

%%====================================================================
%% Internal functions
%%====================================================================
