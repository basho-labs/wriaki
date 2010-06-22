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

%% @doc Convenience functions around article history.
-module(article_history).

-export([add_version/2,
         get_version_summaries/1]).

-include("wriaki.hrl").

%% @spec add_version(wriaki:article()) -> ok
%% @doc Update the history object for Article with an
%%      entry for the revision contained in Article.
add_version(Client, Article) ->
    {ok, Hist} = fetch_or_new(Client, wobj:key(Article)),
    wrc:put(Client,
            wobj:add_link(Hist,
                          {{?B_ARCHIVE, article:archive_key(Article)},
                           date_string(article:get_timestamp(Article))})).

date_string(TS) ->
    integer_to_list(TS).

fetch_or_new(Client, Key) ->
    case wrc:get(Client, ?B_HISTORY, Key) of
        {ok, H} ->
            case wobj:has_siblings(H) of
                true ->
                    {ok, merge_siblings(wobj:get_siblings(H))};
                false ->
                    {ok, H}
            end;
        {error, notfound} ->
            {ok, wobj:create(?B_HISTORY, Key, <<>>)}
    end.

merge_siblings(Siblings) ->
    lists:foldl(fun merge_links/2, hd(Siblings), tl(Siblings)).

merge_links(Obj, Acc) ->
    lists:foldl(fun(L, A) -> wobj:add_link(A, L) end,
                Acc,
                wobj:get_links(Obj)).

-define(TIME_ORDER,
        iolist_to_binary(
          [<<"function(v) {\n">>,
           <<"return v.sort(\n">>,
           <<"function(a,b) { return b[2]-a[2]; }\n">>,
           <<"); }">>])).

-define(RE_URLENCODE,
        iolist_to_binary(
          [<<"function(v) {\n">>,
           <<"return v.map(\n">>,
           <<"function(v) { return [v[0], encodeURIComponent(v[1]), v[2]]; }\n">>,
           <<"); }">>])).

-define(SUMMARY_EXTRACTION,
        iolist_to_binary(
          [<<"function(v) {\n">>,
           <<"var json = JSON.parse(v.values[0].data);\n">>,
           <<"var summary = {\n">>,
           <<"version: json.version,\n">>,
           <<"timestamp: json.timestamp,\n">>,
           <<"message: json.message,\n">>,
           <<"};\n">>,
           <<"var links = v.values[0].metadata.Links;\n">>,
           <<"for(var i in links) {\n">>,
           <<"if (links[i][2] == \"editor\") {\n">>,
           <<"summary.editor = links[i][1];\n">>,
           <<"}\n">>,
           <<"}\n">>,
           <<"return [summary];\n">>,
           <<"}">>])).

get_version_summaries(ArticleKey) ->
    {ok, Client} = wrc:connect(),
    wrc:mapred(Client,
               [{?B_HISTORY, ArticleKey}],
               [{link, <<"archive">>, '_', false},
                %{reduce, {jsanon, ?TIME_ORDER}, <<>>, false}, %TODO: paging
                {reduce, {jsanon, ?RE_URLENCODE}, <<>>, false},
                {map, {jsanon, ?SUMMARY_EXTRACTION}, <<>>, true}]).
