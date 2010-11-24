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
-module(session_resource).

-export([init/1,
         finish_request/2,
         allowed_methods/2,
         expires/2,
         resource_exists/2,
         content_types_provided/2,
         charsets_provided/2,
         to_json/2,
         delete_resource/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("wriaki.hrl").

-record(ctx, {client,
              session}).

init([]) ->
    {ok, Client} = wrc:connect(),
    {ok, #ctx{client=Client}}.

finish_request(RD, Ctx=#ctx{client=C}) ->
    wrc:disconnect(C),
    {true, RD, Ctx#ctx{client=disconnected}}.

allowed_methods(RD, Ctx) ->
    {['HEAD','GET','DELETE'], RD, Ctx}.

expires(RD, Ctx) ->
    {calendar:universal_time(), RD, Ctx}.

resource_exists(RD, Ctx=#ctx{client=C}) ->
    case session:fetch(C, list_to_binary(wrq:path_info(session, RD))) of
        {ok, Session} ->
            case session:get_user(Session) ==
                list_to_binary(
                  mochiweb_util:unquote(wrq:path_info(name, RD))) of
                true ->
                    {ok, SC} = wrc:set_client_id(C, wobj:key(Session)),
                    NewSession = session:refresh(Session),
                    wrc:put(SC, NewSession),
                    {true, RD, Ctx#ctx{session=NewSession, client=SC}};
                false ->
                    {false, RD, Ctx}
            end;
        _ ->
            {false, RD, Ctx}
    end.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

charsets_provided(RD, Ctx) ->
    {[{"utf-8", fun(C) -> C end}], RD, Ctx}.

to_json(RD, Ctx=#ctx{session=Session}) ->
    {mochijson2:encode(
       {struct, [{<<"expiry">>, session:get_expiry(Session)}]}),
     RD, Ctx}.

delete_resource(RD, Ctx=#ctx{client=C, session=Session}) ->
    case wrc:delete(C, ?B_SESSION, wobj:key(Session)) of
        ok ->
            {true, wriaki_auth:clear_cookies(RD), Ctx};
        _ ->
            {false, RD, Ctx}
    end.
