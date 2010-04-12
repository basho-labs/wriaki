-module(session).

-export([fetch/2,
         create/1,
         get_user/1,
         get_expiry/1,
         refresh/1,
         is_valid/1]).
-include("wriaki.hrl").

-define(F_USER, <<"username">>).
-define(F_EXPIRY, <<"expiry">>).

-define(EXTENSION, (60*60*6)). %% expire after 6hrs of inactivity

fetch(Client, Key) ->
    rhc:get(Client, ?B_SESSION, Key).

create(Username) when is_binary(Username) ->
    refresh(rec_obj:create(?B_SESSION, unique_id_62(),
                           {struct, [{?F_USER, Username}]})).

get_user(Session) ->
    rec_obj:get_json_field(Session, ?F_USER).

get_expiry(Session) ->
    rec_obj:get_json_field(Session, ?F_EXPIRY).

refresh(Session) ->
    rec_obj:set_json_field(Session, ?F_EXPIRY, now_secs()+?EXTENSION).

is_valid(Session) ->
    now_secs() < get_expiry(Session).

now_secs() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% @spec unique_id_62() -> string()
%% @doc Create a random identifying integer, returning its string
%%      representation in base 62.
%%      Slightly modified from riak_util
unique_id_62() ->
    Rand = crypto:sha(term_to_binary({make_ref(), now()})),
    <<I:160/integer>> = Rand,
    list_to_binary(base62_list(I)).

base62_list(I) -> base62_list(I,[]).
base62_list(I0, R0) ->
    D = I0 rem 62,
    I1 = I0 div 62,
    R1 = if D >= 36 ->
                 [D-36+$a|R0];
            D >= 10 ->
                 [D-10+$A|R0];
            true ->
                 [D+$0|R0]
         end,
    if I1 =:= 0 ->
            R1;
       true ->
            base62_list(I1, R1)
    end.
