-module(login_form_resource).

-export([init/1,
         to_html/2,
         return_location/1]).
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, nostate}.

to_html(RD, Ctx) ->
    {ok, C} = login_form_dtl:render([{req, wrq_dtl_helper:new(RD)},
                                     {username, ""}]),
    {C, RD, Ctx}.

return_location(RD) ->
    wrq:set_resp_header(
      "Location",
      "/user?next="++mochiweb_util:quote_plus(wrq:raw_path(RD)),
      RD).
