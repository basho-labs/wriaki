
-module(wiki_creole_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    CreoleConfig = [creole, creole, 2,
                    filename:join(code:priv_dir(wiki_creole), "creole_worker.py")],
    Creole = {script_manager,
              {script_manager, start_link, CreoleConfig},
              permanent, 5000, worker, [script_manager, script_worker, creole]},
                 
    {ok, { {one_for_one, 5, 10}, [Creole]} }.

