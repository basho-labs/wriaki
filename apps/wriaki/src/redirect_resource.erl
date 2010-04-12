-module(redirect_resource).
-export([init/1,
         resource_exists/2,
         previously_existed/2,
         moved_permanently/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(Target) ->
    {ok, Target}.

resource_exists(RD, Target) ->
    {false, RD, Target}.

previously_existed(RD, Target) ->
    {true, RD, Target}.

moved_permanently(RD, Target) ->
    {{true, Target}, RD, Target}.
