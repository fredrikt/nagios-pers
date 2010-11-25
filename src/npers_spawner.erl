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
-export([start_link/1,
	 info/0,
	 set_interval/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {wake_up_timer_ref :: timer:tref(),
		wake_up_frequency :: non_neg_integer(),
		stats_timer_ref :: timer:tref(),
		interval_secs :: non_neg_integer(),
		checks_count :: non_neg_integer(),
		all_checks :: list(),
		start_checks :: list(),
		start_per_interval :: non_neg_integer(),
		started_this_interval = 0 :: non_neg_integer(),
		options :: [],
		stats_history = [] :: [{Secs :: non_neg_integer(), Workers :: non_neg_integer(), Count :: non_neg_integer()}]
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

info() ->
    gen_server:call(?SERVER, get_info).

set_interval(Seconds) when is_integer(Seconds) ->
    gen_server:call(?SERVER, {set_interval, Seconds}).

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

    {ok, StatsTRef} = timer:send_interval(Interval * 1000, dump_stats),

    Checks = [],

    State1 = #state{stats_timer_ref = StatsTRef,
		    interval_secs = Interval,
		    options = Options
		   },
    State = set_checks(State1, Checks),

    io:format("Started npers_spawner - will fire ~p checks every ~p seconds.~n",
	      [State#state.start_per_interval, Interval]),

    {ok, State}.

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
    NewState = set_checks (State, Checks),
    {reply, ok, NewState};

handle_call({set_interval, Seconds}, _From, State) when is_integer(Seconds) ->
    NewState = set_checks (State#state{interval_secs = Seconds}, State#state.all_checks),
    {reply, ok, NewState};

handle_call(get_info, _From, State) ->
    Info = [{interval_secs, State#state.interval_secs},
	    {checks_count, State#state.checks_count},
	    {wake_up_frequency, State#state.wake_up_frequency},
	    {start_checks_length, length(State#state.start_checks)},
	    {start_per_interval, State#state.start_per_interval},
	    {started_this_interval, State#state.started_this_interval},
	    {stats_history, State#state.stats_history}
	    ],
    {reply, {ok, Info}, State};

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
handle_info(wake_up, #state{start_per_interval = 0} = State) ->
    %% Ignore, we have not been told what checks to run
    {noreply, State};

handle_info(wake_up, State) ->
    NewState = start_checks(State),
    %% Remove any new signals to wake up that have already arrived, to not
    %% bite our tail when overloaded.
    eat_wake_up(),
    {noreply, NewState};

handle_info(dump_stats, State) ->
    io:format("~p : Started ~p checks the last ~p seconds (goal: ~p)~n",
	      [self(), State#state.started_this_interval, State#state.interval_secs, State#state.checks_count]),
    This = {State#state.interval_secs, State#state.started_this_interval, State#state.checks_count},
    NewHistory1 = [This | State#state.stats_history],
    NewHistory = lists:sublist(NewHistory1, 20),
    NewState = State#state{started_this_interval = 0,
			   stats_history = NewHistory
			  },
    {noreply, NewState};

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
    #state{start_per_interval		= StartNum,
	   start_checks			= SChecks,
	   options			= Options,
	   started_this_interval	= PreviouslyStarted
	  } = State,
    NewSChecks = start_checks2(StartNum, SChecks, State, Options),
    State#state{start_checks		= NewSChecks,
		started_this_interval	= StartNum + PreviouslyStarted
	       }.

start_checks2(Count, [H | T], State, Options) when Count > 0 ->
    npers_worker:start(H, Options),
    start_checks2(Count - 1, T, State, Options);
start_checks2(Count, [], State, Options) ->
    %% restart with all the checks when Count reaches zero
    SChecks = State#state.all_checks,
    start_checks2(Count, SChecks, State, Options);
start_checks2(Count, SChecks, _State, _Options) when Count =:= 0 ->
    %% finished starting workers for this time
    SChecks.

set_checks(State, Checks) when is_list(Checks) ->
    %% Cancel old wake up timer
    case State#state.wake_up_timer_ref of
	undefined ->
	    ok;
	Ref ->
	    {ok, cancel} = timer:cancel(Ref)
    end,

    %% Rig new wake up timer
    Interval = State#state.interval_secs,
    NumChecks = length(Checks),
    {TRef, Freq} =
	if
	    Interval > 0, NumChecks > 0 ->
		WakeEvery = ceiling(Interval * 1000 / NumChecks),
		{ok, WTRef} = timer:send_interval(WakeEvery, wake_up),
		{WTRef, WakeEvery};
	    true ->
		{undefined, undefined}
	end,

    State#state{
      wake_up_timer_ref		= TRef,
      wake_up_frequency		= Freq,
      all_checks		= Checks,
      start_checks		= Checks,
      checks_count		= NumChecks,
      start_per_interval	= 1
     }.

%% from http://schemecookbook.org/Erlang/NumberRounding
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 ->
	    T;
	Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% To try and not bite our own tail, we remove any new 'wake_up's that
%% has arrived while we were processing the last one.
eat_wake_up() ->
    receive
	wake_up ->
	    eat_wake_up()
    after
	0 ->
	    ok
    end.
