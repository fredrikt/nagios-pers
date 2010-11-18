%%%-------------------------------------------------------------------
%%% File    : npers_worker.erl
%%% Author  : Fredrik Thulin <ft@ft-laptop.thulin.net>
%%% Descrip : Run a single Nagios check
%%%
%%% Created : 18 Nov 2010 by Fredrik Thulin <ft@ft-laptop.thulin.net>
%%%-------------------------------------------------------------------
-module(npers_worker).

-behaviour(gen_server).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {cmd :: string(),
		cmd_pid :: pid(),
		start_time
	       }).

-define(TIMEOUT, 60 * 1000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Job) ->
    gen_server:start(?MODULE, Job, []).

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
init(Job) ->
    case Job of
	{nagios_check, Command} when is_list(Command) ->
	    erlang:send_after(10, self(), start),
	    {ok, #state{cmd = Command}, ?TIMEOUT};
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
handle_info(start, State) ->
    Command = State#state.cmd,

    Me = self(),
    StartTime = erlang:now(),
    RunCmd =
	fun() ->
		Output = os:cmd(Command),
		Me ! {cmd_output, self(), Output},
		ok
	end,

    CmdPid = spawn_link(RunCmd),

    {noreply, State#state{cmd_pid = CmdPid,
			  start_time = StartTime
			 },
     ?TIMEOUT
    };

handle_info({cmd_output, Pid, Output}, #state{cmd_pid = Pid} = State) ->
    %%io:format("~p ~s ~s~n", [Pid, State#state.cmd, Output]),
    {stop, normal, State};

handle_info(timeout, State) ->
    {stop, timeout, State};

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
