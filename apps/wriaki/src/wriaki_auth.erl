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
    {ok, Client} = wriaki:riak_client(),
    case rhc:get(Client, ?B_USER, Username) of
        {ok, User} ->
            case user_resource:password_matches(User, Password) of
                true  -> {ok, User};
                false -> false
            end;
        {error, notfound} ->
            false
    end.

check_cookie_auth(SessionCookie) ->
    {ok, C} = wriaki:riak_client(),
    case session:fetch(C, list_to_binary(SessionCookie)) of
        {ok, Session} ->
            case session:is_valid(Session) of
                true ->
                    {ok, UC} = wriaki:riak_client(session:get_user(Session)),
                    rhc:put(UC, session:refresh(Session)),
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
    Username = rec_obj:key(User),
    Session = session:create(Username),
    SessionCookie = rec_obj:key(Session),
    {ok, C} = wriaki:riak_client(Username),
    ok = rhc:put(C, Session),
    {wrq:merge_resp_headers(
      [ mochiweb_cookies:cookie(K, V, [{path, "/"}])
        || {K, V} <- [{?USERNAME_COOKIE, Username},
                      {?SESSION_COOKIE, SessionCookie}] ],
      RD),
     {{cookie, SessionCookie},
      rec_obj:add_link(User, {{rec_obj:bucket(Session),
                               rec_obj:key(Session)},
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
