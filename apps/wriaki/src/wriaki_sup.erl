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
