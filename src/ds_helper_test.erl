-module(ds_helper_test).
-compile({parse_transform, ds_helper}).

-export([go/0, to_atom/1, call_that_returns_d/0]).

go() ->
    DS = #{a=>4, b=>10, c=>45, d=>96},
    PL = maps:to_list(DS),
    C = c,

    10 = DS->b,
    4 = PL->a,
    45 = DS->(C),
    10 = DS->(C),
    96 = PL->(call_that_returns_d()),
    96 = PL->(ds_helper_test:call_that_returns_d()),
    96 = DS->(?MODULE:call_that_returns_d()),
    45 = DS->(to_atom("c")),
    45 = DS->(erlang:apply(ds_helper_test, to_atom, ["c"])),

    [45] = PL -> [c],
    [4, 10] = DS->[a,b],
    [96] = DS->[call_that_returns_d()],
    [96] = PL->[ds_helper_test:call_that_returns_d()],
    %[96] = DS->[?MODULE:call_that_returns_d()],
    [45] = DS->[to_atom("c")],
    [45] = DS->[erlang:apply(ds_helper_test, to_atom, ["c"])],
    [4, 10, 45, 96] = PL -> [a,b,c,d],
    %[4, 10, 45, 96] = PL -> [a,b,?MODULE:to_atom("c"),call_that_returns_d()],
    %[4, 10, 45, 96] = DS -> [a,b,?MODULE:to_atom([$c]),call_that_returns_d()],
    ok.

to_atom(X) ->
    list_to_atom(X).

call_that_returns_d() ->
    d.
