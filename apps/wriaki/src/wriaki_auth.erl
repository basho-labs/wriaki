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
-module(wriaki_auth).

-export([head/0,
         check/1,
         check_user_pass/2,
         start_session/2,
         clear_cookies/1]).

-include("wriaki.hrl").
         
head() -> "Basic realm=\"wriaki\"".

check(RD) ->
    case wrq:get_cookie_value("session", RD) of
        SessionCookie=[_|_] ->
            check_cookie_auth(SessionCookie);
        _ ->
            false
    end.

check_user_pass(Username, Password) ->
    {ok, Client} = wrc:connect(),
    case wrc:get(Client, ?B_USER, Username) of
        {ok, User} ->
            case user_resource:password_matches(User, Password) of
                true  -> {ok, User};
                false -> false
            end;
        {error, notfound} ->
            false
    end.

check_cookie_auth(SessionCookie) ->
    {ok, C} = wrc:connect(),
    case session:fetch(C, list_to_binary(SessionCookie)) of
        {ok, Session} ->
            case session:is_valid(Session) of
                true ->
                    {ok, UC} = wrc:set_client_id(C, session:get_user(Session)),
                    wrc:put(UC, session:refresh(Session)),
                    {ok, User} = wuser:fetch(UC, session:get_user(Session)),
                    {{cookie, SessionCookie}, User};
                false ->
                    false
            end;
        {error, notfound} ->
            false
    end.

-define(USERNAME_COOKIE, "username").
-define(SESSION_COOKIE, "session").

start_session(RD, User) ->
    Username = wobj:key(User),
    Session = session:create(Username),
    SessionCookie = wobj:key(Session),
    {ok, C} = wrc:connect(Username),
    ok = wrc:put(C, Session),
    {wrq:merge_resp_headers(
      [ mochiweb_cookies:cookie(K, V, [{path, "/"}])
        || {K, V} <- [{?USERNAME_COOKIE, Username},
                      {?SESSION_COOKIE, SessionCookie}] ],
      RD),
     {{cookie, SessionCookie},
      wobj:add_link(User, {{wobj:bucket(Session),
                            wobj:key(Session)},
                           now_secs_string()})}}.

now_secs_string() ->
    list_to_binary(
      integer_to_list(
        calendar:datetime_to_gregorian_seconds(
          calendar:universal_time()))).

clear_cookies(RD) ->
    wrq:merge_resp_headers(
      [ mochiweb_cookies:cookie(K, V, [{path, "/"}])
        || {K, V} <- [{?USERNAME_COOKIE, ""},
                      {?SESSION_COOKIE, ""}] ],
      RD).
