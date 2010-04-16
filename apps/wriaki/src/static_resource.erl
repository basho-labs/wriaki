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

-module(static_resource).
-export([init/1,
         resource_exists/2,
         content_types_provided/2,
         generate_body/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {docroot,    %% where our static files live
              fullpath}). %% path to file we'll send

init(DocRoot) -> {ok, #ctx{docroot=DocRoot}}.

resource_exists(RD, Ctx) ->
    Path = wrq:disp_path(RD),
    Rel = filename:join([ P || P <- string:tokens(Path, "/"),
                               P /= ".."]),
    Abs = filename:join([code:priv_dir(wriaki),
                         Ctx#ctx.docroot,Rel]),
    {filelib:is_file(Abs), RD,Ctx#ctx{fullpath=Abs}}.

content_types_provided(RD, Ctx) ->
    Path = wrq:disp_path(RD),
    {[{webmachine_util:guess_mime(Path), generate_body}],
     RD, Ctx}.

generate_body(RD, Ctx) ->
    {ok, Data} = file:read_file(Ctx#ctx.fullpath),
    {Data, RD, Ctx}.
