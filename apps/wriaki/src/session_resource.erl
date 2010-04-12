-module(session_resource).

-export([init/1,
         allowed_methods/2,
         expires/2,
         resource_exists/2,
         content_types_provided/2,
         to_json/2,
         delete_resource/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("wriaki.hrl").

-record(ctx, {client,
              session}).

init([]) ->
    {ok, Client} = wriaki:riak_client(),
    {ok, #ctx{client=Client}}.

allowed_methods(RD, Ctx) ->
    {['HEAD','GET','DELETE'], RD, Ctx}.

expires(RD, Ctx) ->
    {calendar:universal_time(), RD, Ctx}.

resource_exists(RD, Ctx=#ctx{client=C}) ->
    case session:fetch(C, list_to_binary(wrq:path_info(session, RD))) of
        {ok, Session} ->
            case session:get_user(Session) ==
                list_to_binary(wrq:path_info(name, RD)) of
                true ->
                    NewSession = session:refresh(Session),
                    rhc:put(C, NewSession),
                    {true, RD, Ctx#ctx{session=NewSession}};
                false ->
                    {false, RD, Ctx}
            end;
        _ ->
            {false, RD, Ctx}
    end.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx=#ctx{session=Session}) ->
    {mochijson2:encode(
       {struct, [{<<"expiry">>, session:get_expiry(Session)}]}),
     RD, Ctx}.

delete_resource(RD, Ctx=#ctx{client=C, session=Session}) ->
    case rhc:delete(C, ?B_SESSION, rec_obj:key(Session)) of
        ok ->
            {true, wriaki_auth:clear_cookies(RD), Ctx};
        _ ->
            {false, RD, Ctx}
    end.
