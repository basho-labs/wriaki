%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at

%%   http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.    

%% @doc Parameterized module wrapper for Article.  Allows ErlyDTL
%%      templates to access properties of article with dotted
%%      notation.
%%
%%      Article
%%        A riak_object from either the "article" or "archive"
%%        bucket.
%%
%%      V
%%        The article revision to choose (almost always *the*
%%        revision of Article, but useful when Article has
%%        multiple revisions through riak siblings).
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2009 Basho Technologies, Inc.  All Rights Reserved.
-module(article_dtl_helper, [ArticleVs, V]).

-export([key/0,
         path/0,
         encoded_vclock/0,
         text/0,
         html/0,
         msg/0,
         history/0]).
-export([has_multiple/0, tip_versions/0, selected_version/0]).

-include("wriaki.hrl").

%% @type article_props() = proplist()
%% @type history_props() = proplist()
%% @type version() = integer()

%% @spec key() -> binary()
%% @doc get the key of the article
key() -> 
    case rec_obj:bucket(hd(ArticleVs)) of
        ?B_ARTICLE -> rec_obj:key(hd(ArticleVs));
        ?B_ARCHIVE -> article:article_key_from_archive_key(
                        rec_obj:key(hd(ArticleVs)))
    end.

%% @spec path() -> iolist()
%% @doc get the URL-path to the article
path() ->
    article:url(hd(ArticleVs)).

%% @spec encoded_vclock() -> binary()
%% @doc get the vclock for Article, base64-encoded
encoded_vclock() ->
    rec_obj:get_vclock(hd(ArticleVs)).

%% @spec version_data() -> article_props()
%% @doc get the object data associated with version V
version_data() ->
    hd([ A || A <- ArticleVs, V =:= article:get_version(A)]).

%% @spec text() -> binary()
%% @doc get the plain-text format of Article
text() -> 
    article:get_text(version_data()).

%% @spec html() -> string()
%% @doc get the html format of Article
%%      note: this is an html fragment, not a whole HTML document
html() ->
    creole:text2html(text()).

%% @spec msg() -> binary()
%% @doc get the commit message of Article
msg() ->
    article:get_message(version_data()).
    
%% @spec history() -> [history_props()]
%% @doc get the history summary of Article
history() ->
    {ok, Summaries} = article_history:get_version_summaries(key()),
    lists:sort(
      fun(A, B) ->
              proplists:get_value(time, A) > proplists:get_value(time, B)
      end,
      lists:map(
        fun({struct, Props}) ->
                [{version, proplists:get_value(<<"version">>, Props)},
                 {msg, proplists:get_value(<<"message">>, Props)},
                 {editor, proplists:get_value(<<"editor">>, Props)},
                 {time, format_time(proplists:get_value(<<"timestamp">>, Props))}]
        end,
        Summaries)).

%% @spec format_time(datetime()) -> iolist()
%% @doc format a history timestamp for display in HTML
format_time(EpochSecs) ->
    Secs = EpochSecs rem 1000000,
    MegaSecs = (EpochSecs-Secs) div 1000000,
    {{Y,M,D},{H,I,S}} = calendar:now_to_universal_time({MegaSecs, Secs, 0}),
    io_lib:format("~4..0b.~2..0b.~2..0b ~2..0b:~2..0b:~2..0b",
                  [Y,M,D,H,I,S]).

%% @spec has_multiple() -> boolean()
%% @doc true if Article contains multiple revisions (riak-siblings),
%%      false if Article contains just one revision
has_multiple() ->
    length(ArticleVs) > 1.

%% @spec tip_versions() -> [version()]
%% @doc list of revisions contained in Article
tip_versions() ->
    [ article:get_version(A) || A <- ArticleVs ].

%% @spec selected_version() -> version()
%% @doc revision of Article chosen for display
selected_version() -> V.
