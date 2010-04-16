%% -------------------------------------------------------------------
%%
%% Copyright (c) 2009-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Generic server for parallelizing requests to os-processes.
%%
%%  This module attempts to solve the problem of distributing requests
%%  to long-lived stdio-oriented OS-processes.  That is, if you have a
%%  program (shell script, etc.) that you need to communicate with,
%%  which reads data from stdin and returns data on stdout, using this
%%  module (along with {@link script_worker}) should make the
%%  implementation of this communication simple.  Once that
%%  implementation is complete, it should also be trivial to move to
%%  using a pool of these scripts as parallel workers.
%%
%%  Implement your script interface according to the instructions in
%%  {@link script_worker}.
%%
%%  Start the manager and workers by calling {@link start_link/4}.
%%
%%  Submit work by calling {@link process/2}.
%%
%%  If things get confusing, send the message `dump_state' to your
%%  manager process to get it to print out (via {@link error_logger})
%%  its current state.
-module(script_manager).

-behaviour(gen_server).

%% gen_server API
-export([start_link/4]).
%% script_worker API
-export([worker_available/2]).
%% client API
-export([process/2]).
-export([inc_workers/1, dec_workers/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {request_queue,  %% queue()
                worker_stack,   %% [pid()]
                name,           %% atom()
                module,         %% atom()
                script_path,    %% string()
                known_workers}). %% [pid()]

%%====================================================================
%% gen_server API
%%====================================================================

%% @spec start_link(atom(), atom(), integer(), term()) -> {ok,pid()} | ignore | {error,error()}
%% @doc Starts the server
%%    Name: name under which to register the manager server - this is
%%          the Name you'll use to call {@link process/2} later
%%    Module: module that implements the worker logic
%%    Count: number of workers to start
%%    Path: the command-line to run to start the worker
start_link(Name, Module, Count, Path) when is_atom(Name),
                                           is_atom(Module),
                                           is_integer(Count),
                                           is_list(Path) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          {Name, Module, Count, Path},
                          []).

%%====================================================================
%% script_worker API
%%====================================================================

%% @spec worker_available(atom(), pid()) -> ok
%% @doc Register a worker process as ready for work
%%    Name: the registered name of the manager server - same as the
%%          first parameter to {@link start_link/4}
%%    Pid: the process id of the worker
worker_available(Name, Pid) when is_atom(Name), is_pid(Pid) ->
    gen_server:cast(Name, {worker_available, Pid}).

%%====================================================================
%% client API
%%====================================================================

%% @spec process(atom(), term()) -> term()
%% @doc Submit Data to a worker for processing.
%%    Name: the registered name of the manager server - same as the
%%          first parameter to {@link start_link/4}
%%    Data: the data to give the worker
%%  `process/2' will wait for its request to finish or timeout.  If
%%  the request finishes successfully, the retun value is the
%%  response to the request.  If the request times out, the response
%%  is the timeout response from {@link gen_server:call/2}.
process(Name, Data) ->
    gen_server:call(Name, {process, Data}).

%% @spec inc_workers(atom()) -> term()
%% @doc Spin up a new worker for the pool.
inc_workers(Name) ->
    gen_server:call(Name, inc_workers).

%% @spec dec_workers(atom()) -> term()
%% @doc Remove a workers from the pool.
dec_workers(Name) ->
    gen_server:call(Name, dec_workers).

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
init({Name, Module, Count, Path}) ->
    %% trap exits so we can start new worker processes when any of
    %% our initial set dies
    process_flag(trap_exit, true),
    case [ Pid || {ok, Pid} <- [ script_worker:start_link(Name, Module, Path)
                                 || _ <- lists:seq(1, Count) ]] of
        [] ->
            %% if workers failed to start, then fail to start the server
            {stop, no_workers, Name, Module, Path};
        Workers ->
            %% initialize queue, but leave worker stack empty - it
            %% will fill when each worker calls worker_avialable/0
            {ok, #state{request_queue=queue:new(),
                        worker_stack=[], %% workers signal when ready
                        name=Name,
                        module=Module,
                        script_path=Path,
                        known_workers=Workers}}
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
handle_call({process, Data}, From, State) ->
    %% client requested a spellcheck
    case State#state.worker_stack of
        [Worker|Rest] ->
            %% a worker is available - start immediately
            script_worker:process(Worker, Data, From),
            {noreply, State#state{worker_stack=Rest}};
        [] ->
            %% no worker is available - wait in the queue
            {noreply, State#state{request_queue=queue:in({Data, From},
                                                         State#state.request_queue)}}
    end;
handle_call(inc_workers, _From,
            State=#state{name=Name, module=Module, script_path=Path}) ->
    case script_worker:start_link(Name, Module, Path) of
        {ok, Pid} ->
            {reply, ok, State#state{known_workers=[Pid|State#state.known_workers]}};
        _ ->
            {reply, error, State}
    end;
handle_call(dec_workers, _From, State) ->
    case State#state.known_workers of
        [Head|Rest] ->
            %% kill a worker
            script_worker:stop(Head),
            {reply, ok, State#state{known_workers=Rest,
                                    worker_stack=[ Pid || Pid <- State#state.worker_stack,
                                                          Pid /= Head]}};
        _ ->
            %% none to kill
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok, %% don't die if we receive bogus calls
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({worker_available, Pid}, State) ->
    case lists:member(Pid, State#state.known_workers) of
        true ->
            %% a worker process is ready to do something
            case queue:out(State#state.request_queue) of
                {{value, {Data, From}}, Rest} ->
                    %% a job is waiting - start it
                    script_worker:process(Pid, Data, From),
                    {noreply, State#state{request_queue=Rest}};
                {empty, _} ->
                    %% no job is waiting - wait for some
                    {noreply, State#state{worker_stack=[Pid|State#state.worker_stack]}}
            end;
        false ->
            %% this is not a worker we started - likely
            %% left over from a previous manager and will pick
            %% up that manager's EXIT after this message - ignore
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.  %% ignore bogus casts

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    case lists:member(Pid, State#state.known_workers) of
        true ->
            %% a worker died - start a new worker
            error_logger:info_msg("~p:~p noticed ~p:~p crashed - starting new worker~n",
                                  [State#state.name, self(), State#state.module, Pid]),
            {ok, NewPid} = script_worker:start_link(State#state.name,
                                                    State#state.module,
                                                    State#state.script_path),
            %% make sure to take the old worker's pid out of the available workers list
            {noreply, State#state{worker_stack=[ Worker || Worker <- State#state.worker_stack,
                                                           Pid /= Worker ],
                                  known_workers=[NewPid|lists:delete(Pid, State#state.known_workers)]}};
        false -> 
            %% something else we were attached to died
            if Reason /= normal ->
                    %% we had better shutdown ourselves
                    {stop, {trapped_exit, Pid}, State};
               Reason == normal ->
                    %% something died a natural death - ignore
                    {noreply, State}
            end
    end;
handle_info(dump_state, State) ->
    %% debugging convenience - print State to error log
    error_logger:info_msg("~p:~p state: ~p~n", [State#state.name, self(), State]),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}. %% ignore bogus messages

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
