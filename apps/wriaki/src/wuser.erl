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
-module(wuser).

-export([fetch/2,
         create/2,
         set_password/2,
         password_matches/2,
         get_email/1,
         set_email/2,
         get_bio/1,
         set_bio/2
         ]).
-include("wriaki.hrl").

-define(F_PASSWORD, <<"password">>).
-define(F_BIO, <<"bio">>).
-define(F_EMAIL, <<"email">>).

fetch(Client, Key) ->
    rhc:get(Client, ?B_USER, Key).

create(Username, Password) ->
    set_password(
      rec_obj:create(?B_USER, Username, {struct, []}),
      Password).

set_password(User, Password) ->
    rec_obj:set_json_field(User, ?F_PASSWORD, hash_password(User, Password)).

hash_password(User, Password) ->
    Salt = get_salt(),
    base64:encode(crypto:sha([rec_obj:key(User), Password, Salt])).

get_salt() ->
    case application:get_env(wriaki, salt) of
        {ok, Salt} -> Salt;
        undefined  -> "wriaki_salt"
    end.

password_matches(User, Password) ->
    rec_obj:get_json_field(User, ?F_PASSWORD)
        == hash_password(User, Password).

get_email(User) ->
    rec_obj:get_json_field(User, ?F_EMAIL).

set_email(User, Email) when is_binary(Email) ->
    rec_obj:set_json_field(User, ?F_EMAIL, Email).

get_bio(User) ->
    rec_obj:get_json_field(User, ?F_BIO).

set_bio(User, Bio) when is_binary(Bio) ->
    rec_obj:set_json_field(User, ?F_BIO, Bio).
