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
