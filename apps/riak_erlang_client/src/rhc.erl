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
-module(rhc).

-export([create/0, create/4,
         prefix/1,
         get/3, get/4,
         put/2, put/3,
         delete/3, delete/4,
         walk/4,
         mapred/3,
         set_bucket/3
        ]).

-include_lib("raw_http.hrl").

-record(rhc, {ip,
              port,
              prefix,
              options}).

prefix(#rhc{prefix=Prefix}) -> Prefix.

create() ->
    create("127.0.0.1", 8098, "riak", []).

create(IP, Port, Prefix, Opts0) ->
    Opts = case proplists:lookup(client_id, Opts0) of
               none -> [{client_id, random_client_id()}|Opts0];
               _    -> Opts0
           end,
    #rhc{ip=IP, port=Port, prefix=Prefix, options=Opts}.

get(Client, Bucket, Key) ->
    get(Client, Bucket, Key, []).
get(Client, Bucket, Key, Options) ->
    Url = make_url(Client, Bucket, Key, Options),
    case request(get, Url, ["200", "300"]) of
        {ok, _Status, Headers, Body} ->
            {ok, make_rec_obj(Bucket, Key, Headers, Body)};
        {error, {ok, "404", _, _}} ->
            {error, notfound};
        {error, Error} ->
            {error, Error}
    end.

put(Client, Object) ->
    put(Client, Object, []).
put(Client, Object, Options) ->
    case rec_obj:has_siblings(Object) of
        false -> ok;
        true -> throw(cannot_store_siblings)
    end,
    Bucket = rec_obj:bucket(Object),
    Key = rec_obj:key(Object),
    Url = make_url(Client, Bucket, Key, Options),
    Method = if Key =:= undefined -> post;
                true              -> put
             end,
    {Headers0, Body} = serialize_rec_obj(Client, Object),
    Headers = [{?HEAD_CLIENT, option(client_id, Client, Options)}
               |Headers0],
    case request(Method, Url, ["200", "204", "300"], Headers, Body) of
        {ok, Status, ReplyHeaders, ReplyBody} ->
            if Status =:= "204" ->
                    ok;
               true ->
                    {ok, make_rec_obj(Bucket, Key, ReplyHeaders, ReplyBody)}
            end;
        {error, Error} ->
            {error, Error}
    end.

delete(Client, Bucket, Key) ->
    delete(Client, Bucket, Key, []).
delete(Client, Bucket, Key, Options) ->
    Url = make_url(Client, Bucket, Key, Options),
    Headers = [{?HEAD_CLIENT, option(client_id, Client, Options)}],
    case request(delete, Url, ["204"], Headers) of
        {ok, "204", _Headers, _Body} -> ok;
        {error, Error}               -> {error, Error}
    end.

walk(Client, Bucket, Key, Spec) ->
    throw(not_implemented).

mapred(Client, Inputs, Query) ->
    Body = mochijson2:encode(
             {struct, [{<<"inputs">>, mapred_encode_inputs(Inputs)},
                       {<<"query">>, mapred_encode_query(Query)}]}),
    Headers = [{"Content-Type", "application/json"},
               {"Accept", "application/json"}],
    Url = mapred_url(Client),
    case request(post, Url, ["200"], Headers, Body) of
        {ok, "200", ReplyHeaders, ReplyBody} ->
            {ok, mochijson2:decode(ReplyBody)};
        {error, Error} ->
            {error, Error}
    end.

set_bucket(Client, Bucket, Props) ->
    Url = make_url(Client, Bucket, undefined, []),
    Headers =  [{"Content-Type", "application/json"}],
    Body = mochijson2:encode({struct, [{<<"props">>, {struct, Props}}]}),
    case request(put, Url, ["204"], Headers, Body) of
        {ok, "204", _Headers, _Body} -> ok;
        {error, Error}               -> {error, Error}
    end.

%% INTERNAL

root_url(#rhc{ip=Ip, port=Port}) ->
    ["http://",Ip,":",integer_to_list(Port),"/"].

mapred_url(Client) ->
    binary_to_list(iolist_to_binary([root_url(Client), "mapred/"])).

make_url(Client=#rhc{prefix=Prefix}, Bucket, Key, Options) ->
    binary_to_list(
      iolist_to_binary(
        [root_url(Client), Prefix, "/",
         Bucket,"/",
         if Key =/= undefined -> [Key,"/"];
            true              -> []
         end,
         option(extra_rul, Client, Options, []),
         "?",
         qparam(Client, r), qparam(Client, w),
         qparam(Client, dw), qparam(Client, rw),
         qparam(Client, returnbody)])).

qparam(Client, P) ->
    case option(P, Client) of
        undefined -> [];
        Val ->
            [atom_to_list(P),"=",
             if is_integer(Val) -> integer_to_list(Val);
                is_atom(Val)    -> atom_to_list(Val)
             end]
    end.
            
option(O, Client) ->
    option(O, Client, [], undefined).
option(O, Client, Opts) ->
    option(O, Client, Opts, undefined).
option(O, #rhc{options=Options}, Opts, Default) ->
    case proplists:lookup(O, Opts) of
        {O, Val} -> Val;
        _ ->
            case proplists:lookup(O, Options) of
                {O, Val} -> Val;
                _        -> get_app_env(O, Default)
            end
    end.

get_app_env(Env, Default) ->
    case application:get_env(wriaki, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

random_client_id() ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    Id = erlang:phash2([Y,Mo,D,H,Mi,S,node(),NowPart]),
    base64:encode_to_string(<<Id:32>>).

request(Method, Url, Expect) ->
    request(Method, Url, Expect, [], []).
request(Method, Url, Expect, Headers) ->
    request(Method, Url, Expect, Headers, []).
request(Method, Url, Expect, Headers, Body) ->
    Accept = {"Accept", "multipart/mixed, */*;q=0.9"},
    case ibrowse:send_req(Url, [Accept|Headers], Method, Body) of
        Resp={ok, Status, _, _} ->
            case lists:member(Status, Expect) of
                true -> Resp;
                false -> {error, Resp}
            end;
        Error ->
            Error
    end.


make_rec_obj(Bucket, Key, Headers, Body) ->
    Vclock = proplists:get_value(?HEAD_VCLOCK, Headers, ""),
    case ctype_from_headers(Headers) of
        {"multipart/mixed", Args} ->
            {"boundary", Boundary} = proplists:lookup("boundary", Args),
            rec_obj:create_siblings(
              Bucket, Key, Vclock,
              decode_siblings(Boundary, Body));
        {CType, _} ->
            {_, Links, Value} =
                decode_content(Headers, Body),
            rec_obj:set_links(
              rec_obj:set_content_type(
                rec_obj:set_vclock(
                  rec_obj:create(Bucket, Key, Value),
                  Vclock),
                CType),
              Links)
    end.

ctype_from_headers(Headers) ->
    mochiweb_util:parse_header(
      proplists:get_value(?HEAD_CTYPE, Headers)).

decode_siblings(Boundary, "\r\n"++SibBody) ->
    decode_siblings(Boundary, SibBody);
decode_siblings(Boundary, SibBody) ->
    Parts = webmachine_multipart:get_all_parts(
              list_to_binary(SibBody), Boundary),
    [ decode_content([ {binary_to_list(H), binary_to_list(V)}
                       || {H, V} <- Headers ],
                     binary_to_list(Body))
      || {_, {_, Headers}, Body} <- Parts ].

decode_content(Headers, Body) ->
    Links = extract_links(Headers),
    case ctype_from_headers(Headers) of
        {"application/json",_} ->
            {"application/json", Links, mochijson2:decode(Body)};
        {Ctype, _} ->
            {Ctype, Links, Body}
    end.

extract_links(Headers) ->
    {ok, Re} = re:compile("</[^/]+/([^/]+)/([^/]+)>; *riaktag=\"(.*)\""),
    Extractor = fun(L, Acc) ->
                        case re:run(L, Re, [{capture,[1,2,3],binary}]) of
                            {match, [Bucket, Key,Tag]} ->
                                [{{Bucket,Key},Tag}|Acc];
                            nomatch ->
                                Acc
                        end
                end,
    LinkHeader = proplists:get_value(?HEAD_LINK, Headers, []),
    lists:foldl(Extractor, [], string:tokens(LinkHeader, ",")).

serialize_rec_obj(Client, Object) ->
    {make_headers(Client, Object), make_body(Object)}.

make_headers(Client, Object) ->
    [{?HEAD_LINK, encode_links(Client, rec_obj:get_links(Object))},
     {?HEAD_CTYPE, rec_obj:get_content_type(Object)}
     |case rec_obj:get_vclock(Object) of
          ""     -> [];
          Vclock -> [{?HEAD_VCLOCK, Vclock}]
      end].

encode_links(_, []) -> [];
encode_links(#rhc{prefix=Prefix}, Links) ->
    {{FirstBucket, FirstKey}, FirstTag} = hd(Links),
    lists:foldl(
      fun({{Bucket, Key}, Tag}, Acc) ->
              [format_link(Prefix, Bucket, Key, Tag), ", "|Acc]
      end,
      format_link(Prefix, FirstBucket, FirstKey, FirstTag),
      tl(Links)).

format_link(Prefix, Bucket, Key, Tag) ->
    io_lib:format("</~s/~s/~s>; riaktag=\"~s\"",
                  [Prefix, Bucket, Key, Tag]).

make_body(Object) ->
    case rec_obj:get_content_type(Object) of
        "application/json" ->
            mochijson2:encode(rec_obj:get_value(Object));
        _ ->
            rec_obj:get_value(Object)
    end.

mapred_encode_inputs(Inputs) when is_binary(Inputs) ->
    Inputs;
mapred_encode_inputs(Inputs) when is_list(Inputs) ->
    lists:map(
      fun({{Bucket, Key}, KeyData}) -> [Bucket, Key, KeyData];
         ([Bucket, Key, KeyData])   -> [Bucket, Key, KeyData];
         ({Bucket, Key}) -> [Bucket, Key];
         ([Bucket, Key]) -> [Bucket, Key]
      end,
      Inputs).

mapred_encode_query(Query) ->
    lists:map(
      fun({link, Bucket, Tag, Keep}) when is_boolean(Keep) ->
              {struct,
               [{<<"link">>,
                 {struct, [{<<"bucket">>,
                            if is_binary(Bucket) -> Bucket;
                               Bucket =:= '_'    -> <<"_">>
                                                        end},
                           {<<"tag">>,
                            if is_binary(Tag) -> Tag;
                               Tag =:= '_'    -> <<"_">>
                                                     end},
                           {<<"keep">>, Keep}]}}]};
         ({map, {jsanon, Source}, Arg, Keep}) when is_boolean(Keep) ->
              {struct,
               [{<<"map">>,
                 {struct, [{<<"language">>, <<"javascript">>},
                           {<<"source">>, Source},
                           {<<"arg">>, Arg},
                           {<<"keep">>, Keep}]}}]};
         ({reduce, {jsanon, Source}, Arg, Keep}) when is_boolean(Keep) ->
              {struct,
               [{<<"reduce">>,
                 {struct, [{<<"language">>, <<"javascript">>},
                           {<<"source">>, Source},
                           {<<"arg">>, Arg},
                           {<<"keep">>, Keep}]}}]}
      end,
      Query).

                                       
              
    
