
-module(wriaki_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Conf),
        {Mod, {Mod, start, [Conf]}, permanent, 5000, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    wriaki:set_bucket_props(),
    
    Ip = wriaki:get_app_env(web_ip, "0.0.0.0"),
    Port = wriaki:get_app_env(web_port, 8000),
    LogDir = wriaki:get_app_env(log_dir, "priv/log"),

    {ok, Dispatch} = file:consult(filename:join(
                                    code:priv_dir(wriaki),
                                    "dispatch.conf")),

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, LogDir},
                 {dispatch, Dispatch}
                 ],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [webmachine_mochiweb]},

    {ok, {{one_for_one, 5, 10}, [Web]}}.
