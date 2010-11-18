%%%-------------------------------------------------------------------
%%% File    : npers_spawner.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip : The spawner is the process that starts n checks per
%%%           second.
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(npers_spawner).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {interval_timer_ref :: timer:tref(),
		stats_timer_ref :: timer:tref(),
		interval_secs :: non_neg_integer(),
		checks_count :: non_neg_integer(),
		all_checks :: list(),
		start_checks :: list(),
		workers_started :: non_neg_integer()
	       }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Options) when is_list(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Options) when is_list(Options) ->
    Interval = proplists:get_value(interval, Options, 300),

    {ok, TRef} = timer:send_interval(1000, wake_up),

    {ok, StatsTRef} = timer:send_interval(Interval * 1000, dump_stats),

    Checks = [],

    {ok, #state{interval_timer_ref = TRef,
		stats_timer_ref = StatsTRef,
		interval_secs = Interval,
		all_checks = Checks,
		start_checks = Checks,
		checks_count = length(Checks)
	       }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({set_checks, Checks}, _From, State) when is_list(Checks) ->
    NewState =
	State#state{
	  all_checks = Checks,
	  checks_count = length(Checks)
	 },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = not_implemented,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(wake_up, #state{checks_count = 0} = State) ->
    %% Ignore, we have not been told what checks to run
    {noreply, State};

handle_info(wake_up, State) ->
    NewState = start_checks(State),
    {noreply, NewState};

handle_info(dump_stats, State) ->
    io:format("~p : Started ~p checks the last ~p seconds~n",
	      [self(), State#state.workers_started, State#state.interval_secs]),
    {noreply, State#state{workers_started = 0}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_checks(State) ->
    Count = State#state.checks_count,
    SChecks = State#state.start_checks,
    NewSChecks = start_checks2(Count, SChecks, State),
    State#state{start_checks = NewSChecks,
		workers_started = Count
	       }.

start_checks2(Count, [H | T], State) when Count > 0 ->
    npers_worker:start(H),
    start_checks2(Count - 1, T, State);
start_checks2(Count, [], State) ->
    %% restart with all the checks when Count reaches zero
    SChecks = State#state.all_checks,
    start_checks2(Count, SChecks, State);
start_checks2(Count, SChecks, _State) when Count =:= 0 ->
    %% finished starting workers for this time
    SChecks.
