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
-module(wiki_resource).
-export([init/1,
         is_authorized/2,
         resource_exists/2,
         charsets_provided/2,
         to_html/2,
         allowed_methods/2,
         content_types_accepted/2,
         accept_form/2,
         allow_missing_post/2,
         process_post/2,
         finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("wriaki.hrl").

-record(ctx, {
          client, %% riak client
          article_vs, %% article being viewed if tip
          selected_version, %% version of article displayed
          requested_version, %% version of article requested
          archive, %% article being viewed if not tip
          username %% name of user performing edit
         }).

-define(MODE_EDIT,    "edit").
-define(MODE_HISTORY, "history").
-define(MODE_DIFF,    "diff").

init([]) ->
    {ok, Client} = wrc:connect(),
    {ok, #ctx{client=Client}}.

allowed_methods(RD, Ctx) ->
    {['HEAD','GET','POST','PUT'], RD, Ctx}.

charsets_provided(RD, Ctx) ->
    {[{"utf-8", fun(C) -> C end}], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/x-www-form-urlencoded", accept_form}], RD, Ctx}.

is_authorized(RD, Ctx) ->
    Locked = case wrq:method(RD) of
                 'PUT'  -> true;
                 'POST' -> true;
                 _      -> in_mode(RD, ?MODE_EDIT)
             end,
    if Locked ->
            UserCtx = load_username(RD, Ctx),
            if is_binary(UserCtx#ctx.username) ->
                    {true, RD, UserCtx};
               true ->
                    {{halt, 303},
                     login_form_resource:return_location(RD),
                     UserCtx}
            end;
       true -> {true, RD, Ctx}
    end.

load_username(RD, Ctx=#ctx{username=undefined, client=C}) ->
    case wriaki_auth:check(RD) of
        {_AuthType, User} ->
            {ok, UC} = wrc:set_client_id(C, wobj:key(User)),
            Ctx#ctx{username=wobj:key(User),
                    client=UC};
        false ->
            Ctx#ctx{username=none}
    end;
load_username(_, Ctx) -> Ctx.

resource_exists(RD, Ctx=#ctx{client=Client}) ->
    Key = search_path(RD),
    case article:fetch(Client, Key) of
        {ok, ArticleVs} ->
            {true, RD, select_version(RD, Ctx#ctx{article_vs=ArticleVs})};
        {error, notfound} ->
            {false, RD, Ctx}
    end.

select_version(RD, Ctx) ->
    case wrq:get_qs_value("v", RD) of
        ReqV=[_|_] ->
            load_version(list_to_binary(ReqV), Ctx);
        _ ->
            Ctx#ctx{selected_version=latest_version(Ctx#ctx.article_vs)}
    end.

load_version(ReqV, Ctx=#ctx{article_vs=ArticleVs, client=Client}) ->
    case requested_version_in_tip(ArticleVs, ReqV) of
        true ->
            Ctx#ctx{selected_version=ReqV};
        false ->
            CtxV = Ctx#ctx{requested_version=ReqV},
            case article:fetch_archive(Client,
                                       wobj:key(hd(ArticleVs)), ReqV) of
                {ok, Archive} ->
                    CtxV#ctx{selected_version=ReqV,
                             archive=Archive};
                {error, notfound} ->
                    CtxV#ctx{selected_version=latest_version(ArticleVs)}
            end
    end.

latest_version(ArticleVs) ->
    [{V,_}|_] = lists:reverse(
                  lists:keysort(2, [{article:get_version(A),
                                     article:get_timestamp(A)}
                                    || A <- ArticleVs])),
    V.

requested_version_in_tip(ArticleVs, ReqV) ->
    [] /= [ A || A <- ArticleVs, ReqV == article:get_version(A) ].

to_html(RD, Ctx=#ctx{article_vs=ArticleVs, selected_version=V,
                     requested_version=RV, archive=Archive}) ->
    Modes = modes(RD),
    Props = [{req, wrq_dtl_helper:new(RD)},
             {article, article_dtl_helper:new(ArticleVs, V)},
             {mode, Modes},
             {requested_version, RV}
             |if Archive /= undefined ->
                      [{archive, article_dtl_helper:new([Archive], V)}];
                 true ->
                      []
              end],
    {ok, C} = case [ M || {M,true} <- Modes ] of
                  [edit|_] -> article_editor_dtl:render(Props);
                  [history|_] -> article_history_dtl:render(Props);
                  [diff|_] -> article_diff_dtl:render(
                                diff_props(RD, Ctx)++Props);
                  _ -> article_dtl:render(Props)
              end,
    {C, RD, Ctx}.

diff_props(RD, #ctx{article_vs=[A|_], client=C}) ->
    K = wobj:key(A),
    Lv = wrq:get_qs_value("l", RD),
    Rv = wrq:get_qs_value("r", RD),
    {ok, L} = article:fetch_archive(C, K, Lv),
    {ok, R} = article:fetch_archive(C, K, Rv),
    D = diff:diff([ {article:get_version(X), article:get_text(X)}
                    || X <- [L,R] ]),
    [{versions, [{left, Lv}, {right, Rv}]},
     {lines, [ [{type, case Vs of
                           [_,_] -> "common";
                           [V] -> V
                       end},
                {text, T}]
               || {Vs, T} <- D ]}].

accept_form(RD, Ctx=#ctx{client=Client}) ->
    Article = article_from_rd(RD, Ctx),

    %% store archive version
    ok = wrc:put(Client, article:create_archive(Article)),

    %% update history
    ok = article_history:add_version(Client, Article),

    %% store object
    ok = wrc:put(Client, Article),
    {true, RD, Ctx}.

article_from_rd(RD, Ctx) ->
    Body = mochiweb_util:parse_qs(wrq:req_body(RD)),
    Text = list_to_binary(proplists:get_value("text", Body)),
    Msg = list_to_binary(proplists:get_value("msg", Body)),
    Vclock = proplists:get_value("vclock", Body),
    UserCtx = load_username(RD, Ctx),
    article:create(search_path(RD), Text, Msg, Vclock,
                   UserCtx#ctx.username).

allow_missing_post(RD, Ctx) -> {true, RD, Ctx}.

process_post(RD, Ctx) ->
    Article = article_from_rd(RD, Ctx),
    ACtx = Ctx#ctx{article_vs=[Article],
                   selected_version=article:get_version(Article)},
    {Content, _, _} = to_html(RD, ACtx),
    {true,
     wrq:set_resp_header(
       "Content-type", "text/html",
       wrq:set_resp_body(Content, RD)),
     ACtx}.

modes(RD) ->
    M0 = [{edit,    in_mode(RD, ?MODE_EDIT)},
          {history, in_mode(RD, ?MODE_HISTORY)},
          {diff,    in_mode(RD, ?MODE_DIFF)}],
    [{view, [] == [ X || {X, true} <- M0 ]}|M0].

in_mode(RD, ModeName) ->
    wrq:get_qs_value(ModeName, RD) /= undefined.

search_path(RD) ->
    base64url:encode(mochiweb_util:unquote(wrq:disp_path(RD))).

finish_request(RD, Ctx) ->
    case wrq:response_code(RD) of
        404 ->
            {Content, NewRD, NewCtx} =
                case in_mode(RD, ?MODE_EDIT) of
                    true  -> render_404_editor(RD, Ctx);
                    false -> render_404(RD, Ctx)
                end,
            {true,
             wrq:set_resp_header(
               "Content-type", "text/html; charset=utf-8",
               wrq:set_resp_body(Content, NewRD)),
             NewCtx};
        _ ->
            {true, RD, Ctx}
    end.

render_404_editor(RD, Ctx) ->
    Article = article:create(search_path(RD),
                             list_to_binary(
                               [<<"= This page describes ">>,
                                base64url:decode_to_string(search_path(RD)),
                                <<" =\n">>]),
                             <<>>,
                             undefined,
                             <<>>),
    ACtx = Ctx#ctx{article_vs=[Article],
                   selected_version=article:get_version(Article)},
    to_html(RD, ACtx).

render_404(RD, Ctx) ->
    Search = base64url:decode_to_string(search_path(RD)),
    Results = search(Ctx#ctx.client, Search),
    {ok, C} = error_404_dtl:render([{req, wrq_dtl_helper:new(RD)},
                                    {search, Search},
                                    {results, Results}]),
    {C, RD, Ctx}.

search(Client, RawSearch) ->
    case wriaki:search_enabled() of
        true ->
            Search = split_search(sanitize_search(RawSearch)),
            {ok, RawResults} =
                wrc:mapred(Client,
                           {modfun, riak_search, mapred_search,
                            [<<"article">>, iolist_to_binary(Search)]},
                           [{map, {jsanon, search_fun()}, <<>>, true}]),
            case RawResults of
                [{0, Results}] ->
                    [ [{title, base64url:decode(
                                 proplists:get_value(<<"key">>, R))},
                       {ranges, proplists:get_value(<<"ranges">>, R)}]
                      || {struct, R} <- Results ];
                _ ->
                    []
            end;
        false ->
            []
    end.

%% code for search map phase is in priv/mapred/search_map.js
search_fun() ->
    wriaki:get_app_env(search_map,
                       <<"function(v) { return [{key:v.key}]; }">>).

split_search(Search) ->
    Tokens = string:tokens(Search, " "),
    string:join([ [<<"text:">>, T] || T <- Tokens ], " OR ").

sanitize_search(RawSearch) ->
    [ whitespace_search_operator(C) || C <- RawSearch ].

whitespace_search_operator(C) ->
    case is_search_operator(C) of
        true -> 32; % space
        false -> C
    end.

-define(SEARCH_OPERATORS,
        [
         $:,
         $(,
           $),
         ${,
           $},
         $[,
           $],
         $+,
         $-,
         $!,
         $&,
         $|,
         $^,
         $~,
         $*,
         $?,
         $_,
         34  % double quote "
        ]).

is_search_operator(C) ->
    lists:member(C, ?SEARCH_OPERATORS).
