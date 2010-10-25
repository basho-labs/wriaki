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
         search_enabled/0,
         read_mapred_js/0,
         get_app_env/2]).

-include_lib("wriaki.hrl").

set_bucket_props() ->
    {ok, Client} = wrc:connect(),
    ok = wrc:set_bucket(Client, ?B_ARTICLE,
                        [{allow_mult, true}|search_hook()]),
    ok = wrc:set_bucket(Client, ?B_HISTORY, [{allow_mult, true}]).

search_hook() ->
    case search_enabled() of
        true ->
            [{precommit, [{struct,[{<<"mod">>,<<"riak_search_kv_hook">>},
                                   {<<"fun">>,<<"precommit">>}]}]}];
        _ ->
            []
    end.

search_enabled() ->
    get_app_env(search_enabled, false) == true.

get_app_env(Env, Default) ->
    case application:get_env(wriaki, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

%% Read each file in PRIV/mapred/<filename>.js and set its content
%% as wriaki application env <filename>
read_mapred_js() ->
    Dir = filename:join(code:priv_dir(wriaki), "mapred"),
    Filenames = filelib:wildcard("*.js", Dir),
    lists:foreach(
      fun(Name) ->
              {ok, Content} = file:read_file(filename:join(Dir, Name)),
              Atom = list_to_atom(hd(string:tokens(Name, "."))),
              application:set_env(wriaki, Atom, Content)
      end,
      Filenames).
