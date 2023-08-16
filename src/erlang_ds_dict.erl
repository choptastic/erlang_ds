-module(erlang_ds_dict).
-behavior(erlang_ds_type_handler).

-export([
    type/0,
    is_type/1,
    set/3,
    get/3,
    has_key/2,
    delete/2,
    filter/2,
    to_list/1,
    from_list/1,
    from_and_to_this_type/2
]).

type() -> dict.

is_type(Obj) ->
    try dict:size(Obj) of
        _ -> true
    catch _:_ ->
        false
    end.

set(Obj, Key, Val) ->
    dict:store(Key, Val, Obj).

get(Obj, Key, Default) ->
    case dict:find(Key, Obj) of
        {ok, Val} -> Val;
        error -> Default
    end.

has_key(Obj, Key) ->
    dict:is_key(Key, Obj).

delete(Obj, Key) ->
    dict:erase(Key, Obj).

filter(Obj, FilterFun) ->
    dict:filter(FilterFun, Obj).

to_list(Obj) ->
    dict:to_list(Obj).

from_list(Obj) ->
    dict:from_list(Obj).

from_and_to_this_type(Obj, Fun) ->
    List = to_list(Obj),
    Res = Fun(List),
    from_list(Res).
