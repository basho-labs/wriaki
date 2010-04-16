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

%% @doc wrapper around Webmachine's wrq, to make it
%%      directly-accessible to ErlyDTL (and to wrap up
%%      some accessors
-module(wrq_dtl_helper, [ReqData]).
-export([username/0]).

username() -> username(element(1, ReqData)).
username(wm_reqdata) ->
    case wrq:get_resp_header("Set-Cookie", ReqData) of
        Cookie when Cookie /= undefined->
            proplists:get_value(
              "username",
              mochiweb_cookies:parse_cookie(Cookie));
        undefined ->
            wrq:get_cookie_value("username", ReqData)
    end;
username(webmachine_request) ->
    case ReqData:get_out_header("Set-Cookie") of
        Cookie when Cookie /= undefined->
            proplists:get_value(
              "username",
              mochiweb_cookies:parse_cookie(Cookie));
        undefined ->
            ReqData:get_cookie_value("username")
    end.
