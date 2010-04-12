-module(wuser_dtl_helper, [User]).
-export([username/0,
         email/0,
         bio/0]).

username() ->
    rec_obj:key(User).

email() ->
    wuser:get_email(User).

bio() ->
    wuser:get_bio(User).
