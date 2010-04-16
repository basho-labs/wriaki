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
-module(wriaki).

-export([set_bucket_props/0,
         riak_client/0, riak_client/1,
         get_app_env/2]).

-include_lib("wriaki.hrl").

set_bucket_props() ->
    {ok, Client} = wriaki:riak_client(),
    ok = rhc:set_bucket(Client, ?B_ARTICLE, [{<<"allow_mult">>, true}]),
    ok = rhc:set_bucket(Client, ?B_HISTORY, [{<<"allow_mult">>, true}]).

riak_client() ->
    {IP, Port, Prefix} = riak_config(),
    {ok, rhc:create(IP, Port, Prefix, [])}.
riak_client(ClientId) ->
    {IP, Port, Prefix} = riak_config(),
    {ok, rhc:create(IP, Port, Prefix, [{client_id, ClientId}])}.

riak_config() ->
    {get_app_env(riak_ip, "127.0.0.1"),
     get_app_env(riak_port, 8098),
     get_app_env(riak_prefix, "riak")}.

get_app_env(Env, Default) ->
    case application:get_env(wriaki, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.
