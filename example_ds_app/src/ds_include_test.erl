%% All that's happening in this module is testing that the parse transform
%% existing in an include file will also be handled appropriately.

-module(ds_include_test).
-include("ds_syntax.hrl").
-export([go/0]).

go() ->
    L = #{a=>1, b=>2},
    1 = L->a,
    io:format("...parse_transform from -include() passed test.~n").
