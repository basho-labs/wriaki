%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

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
