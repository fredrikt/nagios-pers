%%%-------------------------------------------------------------------
%%% File    : npers_worker.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip : Run a single Nagios check
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(npers_worker).

-behaviour(gen_server).

%% API
-export([start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {host :: nonempty_string(),
		name :: nonempty_string(),
		cmd :: nonempty_string(),
		args :: [nonempty_string()],
		port :: port(),
		output = [],
		start_time,
		options :: [],
		test_mode :: {boolean(), any()}
	       }).

-define(TIMEOUT, 60 * 1000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Job, Options) ->
    gen_server:start(?MODULE, {Job, Options}, []).

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
init({Job, Options}) ->
    TestMode = proplists:get_value(test_mode, Options, {false, undefined}),

    case Job of
	{nagios_check, HostName, CheckName, Command, Args} when is_list(Command), is_list(Args) ->
	    NoEmptyArgs = [X || X <- Args, X /= []],
	    erlang:send_after(10, self(), start),
	    {ok, #state{host		= HostName,
			name		= CheckName,
			cmd		= Command,
			args		= NoEmptyArgs,
			options		= Options,
			test_mode	= TestMode
		       }, ?TIMEOUT};
	_ ->
	    {stop, bad_nagios_check}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_info(start, #state{test_mode = {true, _TestParams}} = State) ->
    %% test mode, don't actually start a worker
    %%io:format("~p TEST: ~s ~p~n", [self(), State#state.cmd, State#state.args]),
    {stop, normal, State};
handle_info(start, State) ->
    StartTime = erlang:now(),

    Port = erlang:open_port({spawn_executable, State#state.cmd},
			    [{args, State#state.args},
			     {line, 10000},
			     exit_status,
			     in,
			     stderr_to_stdout
			    ]),

    {noreply, State#state{port = Port,
			  start_time = StartTime
			 },
     ?TIMEOUT
    };

handle_info({Port, {data, {Flag, Line}}}, #state{port = Port} = State) when Flag =:= eol; Flag =:= noeol ->
    Output = State#state.output ++ Line,
    {noreply, State#state{output = Output}, ?TIMEOUT};

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    #state{host    = HostName,
	   name    = CheckName,
	   output  = Output,
	   options = Options,
	   cmd     = Command,
	   args    = Args
	  } = State,

    FinalOutput = collect_output_lines(State#state.port, Output),

    T1 = State#state.start_time,
    T2 = erlang:now(),
    Seconds = timer:now_diff(T2, T1) div (1000 * 1000),

    if
	Status =/= 0 ->
	    %% show failed checks
	    ArgStr = io_lib:format("~p", [Args]),
	    io:format("~p E:~p ~ps ~s/~s :~ncmd : ~s ~s~nout : ~s~n~n",
		      [self(), Status, Seconds, HostName, CheckName,
		       Command, ArgStr,
		       FinalOutput]);
	Seconds > 20 ->
	    %% show slow checks
	    ArgStr = io_lib:format("~p", [Args]),
	    io:format("~p E:~p ~ps (SLOW) ~s/~s :~ncmd : ~s ~s~nout : ~s~n~n",
		      [self(), Status, Seconds, HostName, CheckName,
		       Command, ArgStr,
		       FinalOutput]);
	true ->
	    ok
    end,

    npers:send_result(HostName, CheckName, Status, FinalOutput, Options),

    {stop, normal, State};

handle_info(timeout, State) ->
    error_logger:error_report([{timeout_after, ?TIMEOUT div 1000},
			       {pid, self()},
			       {host, State#state.host},
			       {service, State#state.name},
			       {command, State#state.cmd},
			       {args, State#state.args}
			      ]),
    {stop, normal, State};

handle_info(Info, State) ->
    {stop, {unknown_signal, Info}, State}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    _ = (catch erlang:port_close(State#state.port)),
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

collect_output_lines(Port, In) when is_list(In) ->
    receive
	{Port, {data, {Flag, Line}}} when Flag =:= eol; Flag =:= noeol ->
	    Output = In ++ Line,
	    collect_output_lines(Port, Output)
    after 0 ->
	    %% no more lines
	    In
    end.
