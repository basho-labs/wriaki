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

%% PUT ==
%%   - signup
%%   - modify
%% POST ==
%%   - login

-module(user_resource).

-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         resource_exists/2,
         charsets_provided/2,
         is_authorized/2,
         to_html/2,
         is_conflict/2,
         accept_form/2,
         process_post/2,
         finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("wriaki.hrl").

-record(ctx, {
          client, %% riak client
          user,   %% user record
          authed, %% whether or not requester is authorized
          auth    %% type of auth found
         }).

init([]) ->
    {ok, Client} = wrc:connect(),
    {ok, #ctx{client=Client}}.

allowed_methods(RD, Ctx) ->
    {['HEAD','GET','PUT','POST'],RD,Ctx}.

charsets_provided(RD, Ctx) ->
    {[{"utf-8", fun(C) -> C end}], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/x-www-form-urlencoded", accept_form}], RD, Ctx}.

resource_exists(RD, Ctx) ->
    NewCtx = lookup_user(RD, Ctx),
    {NewCtx#ctx.user /= notfound, RD, NewCtx}.

is_authorized(RD, Ctx) ->
    UserCtx = lookup_user(RD, Ctx),
    case wrq:method(RD) of
        GETHEAD when GETHEAD == 'GET'; GETHEAD == 'HEAD' ->
            case edit_mode(RD) of
                true ->
                    %% only user can see edit form
                    case wriaki_auth:check(RD) of
                        {_AuthType, User} ->
                            case same_user(User, UserCtx#ctx.user) of
                                true ->
                                    {true, RD, UserCtx};
                                false ->
                                    redirect_to_view(RD, UserCtx)
                            end;
                        _ ->
                            redirect_to_view(RD, UserCtx)
                    end;
                false ->
                    %% anyone can see user's public info
                    {true, RD, UserCtx}
            end;
        _ ->
            {true, RD, UserCtx}
    end.

redirect_to_view(RD, Ctx) ->
    {{halt, 303},
     wrq:set_resp_header("Location", "/user/"++username(RD), RD),
     Ctx}.

is_conflict(RD, Ctx=#ctx{user=notfound}) ->
    {false, RD, Ctx};
is_conflict(RD, Ctx=#ctx{user=User}) ->
    case wriaki_auth:check(RD) of
        {_AuthType, SessionUser} ->
            {same_user(User, SessionUser) =/= true, RD, Ctx};
        _ ->
            {true, RD, Ctx}
    end.

same_user(U1, U2) ->
    wobj:key(U1) == wobj:key(U2).

to_html(RD, Ctx=#ctx{user=User}) ->
    {ok, C} = user_dtl:render([{req, wrq_dtl_helper:new(RD)},
                               {user, wuser_dtl_helper:new(User)},
                               {edit, edit_mode(RD)}]),
    {C, RD, Ctx}.

accept_form(RD, Ctx=#ctx{user=notfound}) ->
    %% register new user
    User = wuser:create(username(RD), []),
    {AuthRD, Auth} = wriaki_auth:start_session(RD, User),
    accept_form(AuthRD, Ctx#ctx{user=User, auth=Auth});
accept_form(RD, Ctx=#ctx{user=User, client=C}) ->
    {ok, Client} = wrc:set_client_id(C, wobj:key(User)),
    ReqProps = mochiweb_util:parse_qs(wrq:req_body(RD)),
    ModUser = update_password(
                update_bio(
                  update_email(User, ReqProps),
                  ReqProps),
                ReqProps),
    ok = wrc:put(Client, ModUser),
    {true, RD, Ctx#ctx{client=Client, user=ModUser}}.

update_email(User, ReqProps) ->
    case proplists:get_value("email", ReqProps) of
        undefined -> User;
        Email -> wuser:set_email(User, list_to_binary(Email))
    end.

update_bio(User, ReqProps) ->
    case proplists:get_value("bio", ReqProps) of
        undefined -> User;
        Bio -> wuser:set_bio(User, list_to_binary(Bio))
    end.

update_password(User, ReqProps) ->
    case proplists:get_value("password", ReqProps) of
        undefined -> User;
        []        -> User;
        Password ->
            %%TODO: kill old sessions
            %%TODO: ask for old password too
            wuser:set_password(User, Password)
    end.

process_post(RD, Ctx=#ctx{user=User, client=C}) ->
    ReqProps = mochiweb_util:parse_qs(wrq:req_body(RD)),
    Password = list_to_binary(proplists:get_value("password", ReqProps, [])),
    case wuser:password_matches(User, Password) of
        true ->
            {NewRD, {_Auth, NewUser}} =
                wriaki_auth:start_session(RD, User),
            {ok, UC} = wrc:set_client_id(C, username(RD)),
            wrc:put(UC, NewUser),
            {true, NewRD, NewUser};
        false ->
            {{halt, 409},
             wrq:set_resp_header(
               "Content-type", "text/plain",
               wrq:set_resp_body(
                 "password incorrect", RD)),
             Ctx}
    end.

lookup_user(RD, Ctx=#ctx{user=undefined, client=Client}) ->
    case wuser:fetch(Client, username(RD)) of
        {ok, User}        -> Ctx#ctx{user=User};
        {error, notfound} -> Ctx#ctx{user=notfound}
    end;
lookup_user(_, Ctx) -> Ctx. %% already looked up

username(RD) ->
    list_to_binary(mochiweb_util:unquote(wrq:path_info(name, RD))).

edit_mode(RD) ->
    wrq:get_qs_value("edit", RD) /= undefined.

finish_request(RD, Ctx) ->
    case wrq:response_code(RD) of
        404 ->
            {ok, Content} = user_404_dtl:render(
                              [{username, username(RD)},
                               {req, wrq_dtl_helper:new(RD)}]),
            {true,
             wrq:set_resp_header(
               "Content-type", "text/html; charset=utf-8",
               wrq:set_resp_body(Content, RD)),
             Ctx};
        _ ->
            {true, RD, Ctx}
    end.
            
