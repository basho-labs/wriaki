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
-module(login_form_resource).

-export([init/1,
         charsets_provided/2,
         to_html/2,
         return_location/1]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, nostate}.

charsets_provided(RD, Ctx) ->
    {[{"utf-8", fun(C) -> C end}], RD, Ctx}.

to_html(RD, Ctx) ->
    {ok, C} = login_form_dtl:render([{req, wrq_dtl_helper:new(RD)},
                                     {username, ""}]),
    {C, RD, Ctx}.

return_location(RD) ->
    wrq:set_resp_header(
      "Location",
      "/user?next="++mochiweb_util:quote_plus(wrq:raw_path(RD)),
      RD).
