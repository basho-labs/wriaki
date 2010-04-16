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
-module(article).

-export([fetch/2,
         fetch_archive/3,
         create/5,
         create_archive/1,
         archive_key/1,
         get_editor/1,
         set_editor/2,
         get_text/1,
         set_text/2,
         get_message/1,
         set_message/2,
         get_version/1,
         get_timestamp/1,
         url/1,
         article_key_from_archive_key/1]).

-include("wriaki.hrl").

-define(L_EDITOR, <<"editor">>).

-define(F_TEXT, <<"text">>).
-define(F_MSG, <<"message">>).
-define(F_VERSION, <<"version">>).
-define(F_TS, <<"timestamp">>).

fetch(Client, Key) ->
    case rhc:get(Client, ?B_ARTICLE, Key) of
        {ok, Object} ->
            case rec_obj:has_siblings(Object) of
                true ->
                    {ok, rec_obj:get_siblings(Object)};
                false ->
                    {ok, [Object]}
            end;
        Error ->
            Error
    end.

fetch_archive(Client, ArticleKey, Version) ->
    rhc:get(Client, ?B_ARCHIVE, archive_key(ArticleKey, Version)).

create(Key, Text, Message, Vclock, Editor)
  when is_binary(Key), is_binary(Text), is_binary(Message),
       is_list(Vclock), is_binary(Editor) ->
    update_version(
      set_text(
        set_message(
          set_editor(
            rec_obj:set_vclock(
              rec_obj:create(?B_ARTICLE, Key, {struct, []}),
              Vclock),
            Editor),
          Message),
        Text)).

create_archive(Article) ->
    set_editor(
      rec_obj:create(?B_ARCHIVE,
                     archive_key(Article),
                     rec_obj:get_value(Article)),
      get_editor(Article)).

archive_key(Article) ->
    archive_key(rec_obj:key(Article), get_version(Article)).
archive_key(ArticleKey, Version) ->
    iolist_to_binary([Version,<<".">>,ArticleKey]).

article_key_from_archive_key(ArchiveKey) ->
    archive_key_part(ArchiveKey, 2).

article_version_from_archive_key(ArchiveKey) ->
    archive_key_part(ArchiveKey, 1).

archive_key_part(ArchiveKey, Part) ->
    {match, [Match]} = re:run(ArchiveKey,
                              "([^.]*)\\.(.*)",
                              [{capture, [Part], binary}]),
    Match.

url(Article) ->
    case rec_obj:bucket(Article) of
        ?B_ARTICLE ->
            ["/wiki/",mochiweb_util:unquote(rec_obj:key(Article))];
        ?B_ARCHIVE ->
            ["/wiki/",mochiweb_util:unquote(
                        article_key_from_archive_key(
                          rec_obj:key(Article)))]
    end.

get_editor(Article) ->
    Links = rec_obj:get_links(Article),
    [Editor] = [ E || {{_, E}, T} <- Links, T =:= ?L_EDITOR],
    Editor.

set_editor(Article, Editor) ->
    rec_obj:add_link(
      rec_obj:remove_links(Article, ?B_USER, ?L_EDITOR),
      {{?B_USER, Editor}, ?L_EDITOR}).

get_text(Article) ->
    rec_obj:get_json_field(Article, ?F_TEXT).
set_text(Article, Text) when is_binary(Text) ->
    update_version(rec_obj:set_json_field(Article, ?F_TEXT, Text)).

get_message(Article) ->
    rec_obj:get_json_field(Article, ?F_MSG).
set_message(Article, Message) when is_binary(Message) ->
    update_version(rec_obj:set_json_field(Article, ?F_MSG, Message)).

get_version(Article) ->
    rec_obj:get_json_field(Article, ?F_VERSION).

get_timestamp(Article) ->
    rec_obj:get_json_field(Article, ?F_TS).

update_version(Article) ->
    {MS, S, _US} = now(),
    TS = 1000000*MS+S,
    rec_obj:set_json_field(
      rec_obj:set_json_field(Article, ?F_TS, TS),
      ?F_VERSION, list_to_binary(
                    mochihex:to_hex(erlang:phash2({get_text(Article),
                                                   get_message(Article),
                                                   TS})))).
