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

%% @doc Diff text versions.
-module(diff).

-export([diff/1]).

diff(Versions) -> diff(Versions, []).

diff([{V,T0}|Rest], Acc) ->
    T = if is_binary(T0) -> binary_to_list(T0);
           is_list(T0) -> T0
        end,
    Lines = string:tokens(T, "\r\n"),
    diff(Rest, diff2(V, Lines, Acc, []));
diff([], Acc) -> Acc.

diff2(V, [L|RestNew], RestAcc, RevAcc) ->
    case lists:splitwith(fun({_,AL}) -> L /= AL end, RestAcc) of
        {_,[]} ->
            diff2(V, RestNew, RestAcc, [{[V], L}|RevAcc]);
        {Before,[{P,L}|After]} ->
            diff2(V, RestNew, After,
                  [{[V|P], L}|lists:reverse(Before)++RevAcc])
    end;
diff2(_, [], RestAcc, RevAcc) ->
    lists:reverse(lists:append(lists:reverse(RestAcc), RevAcc)).
