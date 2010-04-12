%% Diff text versions.
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
