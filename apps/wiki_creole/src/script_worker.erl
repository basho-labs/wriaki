%% @author Bryan Fink <bryan@basho.com>
%% @since 8.Apr.2009
%% @doc Worker server for {@link script_manager} system.
%%
%%  `script_worker' is a generic port-handler for communicating with
%%  os-processes that 1) can be given a batch of data to process over
%%  stdin, and 2) respond with their results as lines printed to
%%  stdout.
%%
%%  To implement your script interface, write a module with the
%%  following four functions:
%%
%%  `init_trigger/0' - should return `none' if your script is ready
%%                     to receive data as soon as it is started, or
%%                     `process_output' if your script emits some
%%                     data before it is ready
%%
%%  `handle_init/1' - only needs to be implemented if `init_trigger/0'
%%                    returned `process_output', and should return
%%                    `done' with the script is finally ready to
%%                    receive a request, or `continue' if the script
%%                    is still initing.  The parameter to `handle_init'
%%                    is the last line of output read from the script
%%
%%  `process/2' - will be called when the script manager chooses this
%%                worker to process data.  The first parameter is the
%%                port() that the script is connected to.  The
%%                second argument is the argument that was given to
%%                {@link script_manager:process/2}.  `process/2' use
%%                {@link erlang:port_command/2} to send the request
%%                to the script.  The return value of `process/2'
%%                is considered opaque, and will be passed verbatim
%%                to `handle_data/2'.
%%
%%  `handle_data/2' - will be called whenever the script prints a line
%%                    to stdout.  The first parameter is the current
%%                    opaque data for the request, and the second
%%                    parameter is the line read from the port.
%%                    'handle_data/2' should return `{done, Response}'
%%                    if the request has completed - `Response' will
%%                    be returned to the caller of
%%                    {@link script_manager:process/2}.
%%                    `handle_data/2' should return
%%                    `{continue, NewOpaque}' if the request is still
%%                    processing - `NewOpaque' will be handed back
%%                    to `handle_data/2' with the next line read.
-module(script_worker).

-behaviour(gen_server).

%% behaviour
-export([behaviour_info/1]).

%% API
-export([start_link/3]).
-export([process/3]).
-export([stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {manager, module, port, state}).
-record(processing, {when_done, partial, opaque, from}).

%% behaviour
behaviour_info(callbacks) ->
    [{init_trigger,0}, {handle_init,1}, {process,2}, {handle_data,2}].

%%====================================================================
%% script_manager/gen_server API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Manager, Module, ScriptPath) ->
    gen_server:start_link(?MODULE, {Manager, Module, ScriptPath}, []).

%% @spec stop(pid()) -> ok
%% @doc Ask the worker to stop.  The worker should continue processing
%%      the request it was given if it has not finished it yet.
stop(Worker) ->
    gen_server:cast(Worker, stop).

%%====================================================================
%% client API
%%====================================================================

%% @spec(pid(), term(), tuple()) -> ok
%% @doc submit Data to Worker for processing, and request that the
%%      response be sent to From.  From should be the "From" handed
%%      to {@link script_manager:handle_call/3}
process(Worker, Data, From) when is_pid(Worker), is_tuple(From) ->
    gen_server:cast(Worker, {process, Data, From}).

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
init({Manager, Module, ScriptPath}) ->
    %% trap exits so we know if the port closes unexpectedly
    process_flag(trap_exit, true),
    %% open port and enter init state to handle aspell startup garbage
    State = #state{manager=Manager,
                   module=Module,
                   port=open_port({spawn, ScriptPath},
                                  [use_stdio, {line, 8192}]),
                   state=init},
    case Module:init_trigger() of
        none ->
            {ok, State, 1}; %% register with manager separately
        process_output ->
            {ok, State} %% register with manager when ready
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
    Reply = ok, %% everything is cast or info here, but don't die on bogus calls
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({process, Data, From}, State=#state{state=idle, port=Port,
                                                module=Module}) ->
    %% handle a request to process some html
    Opaque = Module:process(Port, Data),
    %% wait for replies from the port
    {noreply, State#state{state=#processing{when_done=idle,
                                            partial=[],
                                            opaque=Opaque,
                                            from=From}}};
handle_cast({process, _, From}, State=#state{module=Module}) ->
    %% check request while in an invalid state - don't die
    error_logger:warning_msg("~p:~p received process request while processing another query",
                             [Module, self()]),
    gen_server:reply(From, request_collision),
    {noreply, State};
handle_cast(stop, State=#state{state=idle}) ->
    {stop, normal, State};
handle_cast(stop, State=#state{state=P=#processing{}}) ->
    {noreply, State#state{state=P#processing{when_done=normal}}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Port, {data, {noeol, Data}}},
            State=#state{state=P=#processing{partial=Partial}, port=Port}) ->
    %% queue up data until the end of the line comes
    {noreply, State#state{state=P#processing{partial=[Data|Partial]}}};
handle_info({Port, {data, {eol, Data}}},
            State=#state{state=P=#processing{}, port=Port, module=Module}) ->
    %% end of line, append to queued-up no-endline data
    Line = lists:flatten(lists:reverse([Data|P#processing.partial])),
    case Module:handle_data(P#processing.opaque, Line) of
        {done, Response} ->
            %% work is done
            gen_server:reply(P#processing.from, Response),
            if P#processing.when_done == idle ->
                    %% register for more work
                    script_manager:worker_available(State#state.manager, self()),
                    {noreply, State#state{state=idle}};
               true ->
                    %% we were waiting to die - die now
                    {stop, P#processing.when_done, State}
            end;
        {continue, NewOpaque} ->
            %% work is not done
            {noreply, State#state{state=P#processing{partial=[],
                                                     opaque=NewOpaque}}}
    end;
handle_info({Port, {data, {eol, Data}}},
            State=#state{state=init, module=Module, manager=Manager, port=Port}) ->
    %% init data (like banners, etc. most often)
    NewState = case Module:handle_init(Data) of
                   done ->
                       %% register with manager after init is done
                       script_manager:worker_available(Manager, self()),
                       idle;
                   continue -> init
               end,
    {noreply, State#state{state=NewState}};
handle_info({'EXIT', Port, _}, State=#state{port=Port}) ->
    %% aspell os-process died - stop this process
    case State#state.state of
        #processing{from=From} ->
            %% if we were processing something, let requester know we failed
            %% (instead of just letting them time out)
            gen_server:reply(From, request_failed);
        _ -> ok
    end,
    {stop, port_closed, State#state{port=closed}};
handle_info({'EXIT', Pid, _Reason}, State) ->
    %% some other process (likely the manager) died
    case State#state.state of
        idle ->
            %% we weren't doing anything, so just die
            {stop, {trapped_exit, Pid}, State};
        P=#processing{} ->
            %% attempt to finish the request we were processing before dying
            {noreply, State#state{state=P#processing{when_done={trapped_exit, Pid}}}}
    end;
handle_info(timeout, State=#state{manager=Manager, state=init}) ->
    %% register with manager after init is done
    script_manager:worker_available(Manager, self()),
    {noreply, State#state{state=idle}};
handle_info(_, State) ->
    {noreply, State}. %% ignore bogus messages

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{port=Port}) when is_port(Port) ->
    %% shutdown the port
    port_close(Port),
    receive
        {Port, closed} -> ok %% port closed successfully
    after
        1000 -> {error, timeout} %% port hung
    end;
terminate(_Reason, _State) ->
    %% port was closed already
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
