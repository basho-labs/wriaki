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
-module(creole).
-behaviour(script_worker).

%% user API
-export([text2html/1]).

%% script_worker API
-export([init_trigger/0,
         handle_init/1,
         process/2,
         handle_data/2]).

%% user API

%% @spec text2html(iolist()) -> iolist()
%% @doc Compile Wiki-creole-syntax text into HTML.
text2html(Text) ->
    script_manager:process(creole, Text).

%% script_worker API

-define(COMMAND_BREAK, "------wriaki-creole-break------").

%% @private
init_trigger() -> none.

%% @private
handle_init(_) -> exit("creole does not use handle_init").
    
%% @private
process(Port, Text) ->
    port_command(Port, Text),
    port_command(Port, ["\n", ?COMMAND_BREAK, "\n"]),
    [].

%% @private
handle_data(RespAcc, ?COMMAND_BREAK) ->
    {done, lists:flatten(lists:reverse(RespAcc))};
handle_data(RespAcc, Line) ->
    {continue, ["\n",Line|RespAcc]}.
