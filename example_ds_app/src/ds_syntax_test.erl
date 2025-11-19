-module(ds_syntax_test).
-compile({parse_transform, ds_syntax}).

-export([go/0, to_atom/1, call_that_returns_d/0]).

go() ->
    DS = #{a=>4, b=>10, c=>45, d=>96, "string"=>"whatever", {tuple}=>{tuple_value}},
    PL = maps:to_list(DS),
    C = c,

    10 = DS->b,
    4 = PL->a,
    45 = DS->(C),
    45 = PL->((C)),
    96 = PL->(call_that_returns_d()),
    96 = PL->(ds_syntax_test:call_that_returns_d()),
    96 = DS->(?MODULE:call_that_returns_d()),
    45 = DS->(to_atom("c")),
    45 = DS->(erlang:apply(ds_syntax_test, to_atom, ["c"])),
    "whatever" = DS->"string",
    {tuple_value} = PL->{tuple},
    "" = DS->no_key,

    [45] = PL -> [c],
    [4, 10] = DS->[a,b],
    [96] = DS->[call_that_returns_d()],
    [96] = PL->[ds_syntax_test:call_that_returns_d()],
    [96] = DS->[?MODULE:call_that_returns_d()],
    [45] = DS->[to_atom("c")],
    [45] = DS->[erlang:apply(ds_syntax_test, to_atom, ["c"])],
    [4, 10, 45, 96] = PL -> [a,b,c,d],
    [4, 10, 45, 96] = PL -> [a,b,begin ?MODULE:to_atom("c") end,call_that_returns_d()],
    [4, 10, 45, 96] = DS -> [a,b,?MODULE:to_atom([$c]),call_that_returns_d()],
    ["whatever"] = DS->["string"],
    [{tuple_value}] = DS->[{tuple}],
    [{tuple_value}, "whatever"] = DS->[{tuple}, "string"],
    F1 = fun() ->
        10 = DS->b,
        4 = PL->a
    end,
    F1(),
    (fun() ->
        case true of
            true ->
                10 = DS->b,
                [45] = PL->[c]
        end
    end)(),

    %% Testing that it doesn't incorrectly mark the X-> as a lookup
    X = 1,
    case true of
        true when X==X->
            ok
    end,

%    Y = Y,
%    if
%        Y==Y-> ok
%    end,
    
    ok.

to_atom(X) ->
    list_to_atom(X).

call_that_returns_d() ->
    d.
