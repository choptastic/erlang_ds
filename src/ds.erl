%% Copyright Jesse Gumm 2023
%% MIT License
-module(ds).

%% create custom type handlers that aren't `list` or `map`
-export([
    register_type_handler/1,
    unregister_type_handler/1
]).

%% create custom update
-export([
    register_updater/2,
    unregister_updater/1
]).

-export([
    set/2,set/3,
    get/2,get/3,
    get_list/2,get_list/3,
    has_key/2,
    delete/2,
    delete_list/2,
    update/3,
    map/2,
    filter/2,
    rekey/2,
    rekey/3,
    keep/2,
%   qry/2,
    boolize/2,
    atomize/2,
    transform/2,
    compare/3,
    merge/1,
    merge/2,
    guess_merge/1,
    guess_merge/2,
    type/1,
    to_list/1,
    to_map/1,
    to_type/2
]).

-define(MOD(V), (get_type_handler(V))).
-define(IS_BLANK(V), (V==undefined orelse V=="" orelse V==<<>>)).
-define(IS_PROPLIST(V), (is_list(V) andalso is_tuple(hd(V)) andalso tuple_size(hd(V))==2)).

-type object() :: list() | map() | any().
-type key() :: any().
-type keys() :: [key()].
-type value() :: any().
-type values() :: [value()].
-type default_value() :: value().
-type key_value_tuple() :: {key(), value()}.
-type key_value_tuples() :: [key_value_tuple()].
-type updater_key() :: atom() | tuple().
-type updater_mod_fun() :: {atom(), atom()}.
-type update_action() :: function() | updater_mod_fun() | updater_key().
-type transform_tuple() :: {update_action(), keys()}.
-type transform_list() :: [transform_tuple()].
-type key_map() :: [{key(), key()}].
-type unmerged_object() :: object().
-type proplist() :: [{key(), value()}].
-type obj_type() :: list | map | atom().

-export_type([
    object/0,
    key/0,
    value/0,
    default_value/0,
    key_value_tuple/0,
    updater_key/0,
    updater_mod_fun/0,
    proplist/0,
    obj_type/0
]).

register_updater(UpdaterKey, MFA) ->
    erlang_ds_register:register_updater(UpdaterKey, MFA).

unregister_updater(UpdaterKey) ->
    erlang_ds_register:unregister_updater(UpdaterKey).

register_type_handler(Module) ->
    erlang_ds_register:register_type_handler(Module).


unregister_type_handler(Module) ->
    erlang_ds_register:unregister_type_handler(Module).

-spec set(object(), key(), value()) -> object().
set(Obj,Key,Val) ->
    set(Obj,{Key,Val}).

-spec set(object(), key_value_tuple() | key_value_tuples()) -> object().
set(Obj,{Key,Val}) when is_list(Obj) ->
    % delete a matching key if exists and prepend {K,V} to the list. No guarantee of proplist order
    [{Key,Val} | delete(Obj,Key)];
set(Obj,{Key,Val}) when is_map(Obj) ->
    maps:put(Key, Val, Obj);
set(Obj,{Key,Val}) ->
    ?MOD(Obj):set(Obj, Key, Val);
set(Obj,[]) ->
    Obj;
set(Obj,[{Key,Val} | Rest]) ->
    NewObj = set(Obj,{Key,Val}),
    set(NewObj,Rest).

% Will get a list of values in the order specified in the "Keys" argument
-spec get_list(object(), keys(), default_value()) -> values().
get_list(Obj,Keys,Default) when is_list(Keys) ->
    [get(Obj,Key,Default) || Key<-Keys].

-spec get_list(object(), keys()) -> values().
get_list(Obj,Keys) when is_list(Keys) ->
    get_list(Obj,Keys,"").

% Get a single value using the key "Key" and if not found, return "Default"
-spec get(object(), key(), default_value()) -> value().
get(Obj,Key,Default) when is_list(Obj) ->
    proplists:get_value(Key,Obj,Default);
get(Obj,Key,Default) when is_map(Obj) ->
    maps:get(Key,Obj,Default);
get(Obj,Key,Default) ->
    ?MOD(Obj):get(Obj, Key, Default).

-spec get(object(), key()) -> value().
get(Obj,Key) ->
    get(Obj,Key,"").

-spec has_key(object(), key()) -> boolean().
has_key(Obj,Key) when is_list(Obj) ->
    lists:keymember(Key,1,Obj);
has_key(Obj,Key) when is_map(Obj) ->
    maps:is_key(Key, Obj);
has_key(Obj,Key) ->
    ?MOD(Obj):has_key(Obj, Key).

-spec update(object(), key() | keys(), update_action()) -> object().
update(Obj,[],_Updater) ->
    Obj;
update(Obj,[Key|RestKeys],Updater) ->
    NewObj = update(Obj,Key,Updater),
    update(NewObj,RestKeys,Updater);
update(Obj,Key,Updater) when is_function(Updater) ->
    NewVal = Updater(get(Obj,Key)),
    set(Obj,Key,NewVal);
update(Obj, Key, UpdaterKey) ->
    %% Warning, potential for infinite loop here
    Updater = updater_from_term(UpdaterKey),
    update(Obj, Key, Updater).

-spec updater_from_term(updater_mod_fun() | updater_key()) -> fun().
updater_from_term(UpdaterKey = {Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    %% This is a special handler for {Mod,Fun}, but if Mod:Fun/1 isn't a
    %% function, then treat it as an updater key
    case erlang:function_exported(Mod, Fun, 1) of
        true ->
            fun Mod:Fun/1;
        false ->
            erlang_ds_register:get_updater(UpdaterKey)
    end;
updater_from_term(UpdaterKey) ->
    erlang_ds_register:get_updater(UpdaterKey).

-spec boolize(object(), keys()) -> object().
boolize(Obj,Keys) ->
    update(Obj,Keys,fun(V) ->
        if
            V==false;
            V==0;
            V=="0";
            V==undefined;
            V=="" -> false;
            true -> true
        end
    end).

%% DateFormat can be date, unixtime, or a binary/string formatted for use with qdate, or any other term registered as a format with qdate
%% Requires qdate installed.
%% If DateFormat cannot be handled, will instead 
-spec format_date(object(), keys(), DateFormat :: unixtime | date | now | any()) -> object().
format_date(Obj, Keys, DateFormat) ->
    UpdateFun = case DateFormat of
        unixtime -> fun(D) -> try qdate:to_unixtime(D) catch _:_ -> 0 end end;
        date -> fun(D) -> try qdate:to_date(D) catch _:_ -> {{1970,1,1},{0,0,0}} end end;
        now -> fun(D) -> try qdate:to_now(D) catch _:_ -> {0,0,0} end end;
        Format -> fun(D) -> try qdate:to_string(Format, D) catch _:_ -> "Invalid Format" end end
    end,
    update(Obj, Keys, UpdateFun).

-spec atomize(object(), keys()) -> object().
atomize(Obj,Keys) ->
    update(Obj,Keys,fun
        (V) when is_list(V) -> list_to_atom(V);
        (V) when is_atom(V) -> V;
        (V) when is_binary(V) -> list_to_atom(binary_to_list(V));
        (V) when is_integer(V) -> list_to_atom(integer_to_list(V));
        (V) -> throw({cannot_convert_to_atom,V})
    end).

-spec transform(object(), transform_list()) -> object().
transform(Obj,{date, Keys}) ->
    format_date(Obj, Keys, date);
transform(Obj,{unixtime, Keys}) ->
    format_date(Obj, Keys, unixtime);
transform(Obj,{now, Keys}) ->
    format_date(Obj, Keys, now);
transform(Obj,{{date,DateFormat}, Keys}) ->
    format_date(Obj, Keys, DateFormat);

transform(Obj,{boolize,Keys}) ->
    boolize(Obj,Keys);
transform(Obj,{atomize,Keys}) ->
    atomize(Obj,Keys);
transform(Obj,{Fun,Keys}) when is_function(Fun) ->
    update(Obj,Keys,Fun);
transform(Obj,{DateFormat, Keys}) ->
    format_date(Obj, Keys, DateFormat);

transform(Obj,Map) when is_list(Map) ->
    lists:foldl(fun(Action,Acc) ->
        transform(Acc,Action)
    end,Obj,Map).

-spec map(object(), fun()) -> object().
map(Obj,Fun) when is_list(Obj) andalso is_function(Fun, 1) ->
    [{Key,Fun(Val)} || {Key,Val} <- Obj];
map(Obj,Fun) when is_list(Obj) andalso is_function(Fun, 2) ->
    [{Key,Fun(Key, Val)} || {Key,Val} <- Obj];
map(Obj,Fun) when (is_function(Fun, 1) orelse is_function(Fun, 2)) ->
    as_list(Obj, fun(List) -> map(List, Fun) end).

-spec rekey(object(), FromKey :: key(), ToKey :: key()) -> object().
rekey(Obj,FromKey,ToKey) ->
    rekey(Obj,[{FromKey,ToKey}]).

-spec rekey(object(), key_map()) -> object().
rekey(Obj,KeyMap) when is_list(Obj) ->
    lists:map(fun({K,V}) ->
        {rekey_swapkey(K,KeyMap),V}
    end,Obj);
rekey(Obj,KeyMap) ->
    as_list(Obj, fun(List) -> rekey(List, KeyMap) end).

rekey_swapkey(Current,KeyMap) ->
    %% Since the KeyMap is itself a proplist of {oldkey,newkey},
    %% let's find oldkey and return it's "value", and if it's not found, just
    %% return itself
    get(KeyMap,Current,Current).


% TODO: improve performance by only traversing main list once. Fast enough for now
% Maybe just use filter/2.
-spec delete_list(object(), keys()) -> object().
delete_list(Obj, []) ->
    Obj;
delete_list(Obj,[Key|RestKeys]) ->
    NewObj = delete(Obj,Key),
    delete_list(NewObj,RestKeys).

-spec delete(object(), key()) -> object().
delete(Obj,Key) when is_list(Obj) ->
    lists:keydelete(Key, 1, Obj);
delete(Obj,Key) when is_map(Obj) ->
    maps:remove(Key, Obj);
delete(Obj,Key) ->
    ?MOD(Obj):delete(Obj, Key).


% Opposite of delete_list(): keeps all listed keys and removes all others
-spec keep(object(), keys()) -> object().
keep(Obj,Keys) ->
    FilterFun = fun(Key,_Val) ->
        lists:member(Key,Keys)
    end,
    filter(Obj, FilterFun).

-spec filter(object(), fun()) -> object().
filter(Obj, Fun) when is_list(Obj) ->
    %% maps:filter use fun(K,V), while lists:filter would use
    %% fun({K,V}), so we need a translation
    ListFun = fun({K,V}) -> Fun(K,V) end,
    lists:filter(ListFun, Obj);
filter(Obj, Fun) when is_map(Obj) ->
    maps:filter(Fun, Obj);
filter(Obj, Fun) ->
    ?MOD(Obj):filter(Obj, Fun).
    
merge_full(ExtendedProplist) when ?IS_PROPLIST(ExtendedProplist) ->
    lists:foldl(fun
        ({_,Val},Acc) when ?IS_BLANK(Val) ->
                %% Ignore all values of ""
                Acc;
        ({Key,Val},{Merged,Unmerged}) ->
            case get(Unmerged,Key,undefined) of
                undefined ->
                    %% This key is not flagged as an unmergable one, so we try it out
                    case get(Merged,Key) of
                        NewV when ?IS_BLANK(NewV) -> 
                            NewMerged = set(Merged,Key,Val),
                            {NewMerged,Unmerged};
                        Val ->  
                            %% Value matches the existing value exactly, do nothing
                            {Merged,Unmerged};
                        Other ->
                            %current value doesn't match already recorded value
                            % this is a conflict
                            NewMerged = delete(Merged,Key),
                            NewUnmerged = set(Unmerged,Key,[Other,Val]),
                            {NewMerged,NewUnmerged}
                    end;
                UnmergedVals ->
                    NewUnmerged = set(Unmerged,Key,[Val|UnmergedVals]),
                    {Merged,NewUnmerged}
            end
    end,{[],[]},ExtendedProplist).
 
-spec merge(object(), object()) -> {object(), unmerged_object()}.
merge(A,B) ->
    merge([A,B]).
    
%% Returns a two_tuple: {Merged, Unmerged}
%% Merged is the new merged result
-spec merge([object()]) -> {object(), unmerged_object()}.
merge(List) ->
    Type = type(hd(List)),
    List2 = lists:flatten([to_list(X) || X <- List]),
    {Merged, Unmerged} = merge_full(List2),
    {to_type(Type, Merged), to_type(Type, Unmerged)}.

%% Returns a single proplist with a guess of the final proplist.
%% For each field, just returns the first value it encounters, even if it's not the same
-spec guess_merge([object()]) -> object().
guess_merge([]) ->
    [];
guess_merge(List) when ?IS_PROPLIST(List) ->
    FinalDict = lists:foldl(fun({K,V},Dict) ->
        case not(dict:is_key(K,Dict)) 
                    andalso V=/=undefined 
                    andalso has_non_whitespace(V)  of
            true -> dict:store(K,V,Dict);
            false -> Dict
        end
    end,dict:new(),List),
    dict:to_list(FinalDict);
guess_merge(Objs) ->
    ToType = type(hd(Objs)),
    FlatProplist = lists:flatten([to_list(Obj) || Obj <- Objs]),
    Merged = guess_merge(FlatProplist),
    to_type(ToType, Merged).

-spec guess_merge(object(), object()) -> object().
guess_merge(A,B) ->
    guess_merge([A,B]).

-define(IS_WS(WS), WS=:=10;WS=:=13;WS=:=32;WS=:=9;WS=:=[];WS=:=<<>>).

has_non_whitespace([]) ->
    false;
has_non_whitespace(<<>>) ->
    false;
has_non_whitespace(<<WS,T/binary>>) when ?IS_WS(WS) ->
    has_non_whitespace(T);
has_non_whitespace([WS | T]) when ?IS_WS(WS) ->
    has_non_whitespace(T);
has_non_whitespace(_) ->
    true.



% List of Proplists, returns the proplists that match the query values (specified as a tuple of {Key,Val} or a list of those tuples, in which case they must all match
% no matches returns an empty list ( [] )
%TODO: finish this
%qry(ObjL,{SearchKey,SearchVal}) ->
%   [].

less_equal(A,B) ->
    if
        A==B -> equal;
        A<B -> true;
        A>B -> false
    end.

greater_equal(A,B) ->
    if
        A==B -> equal;
        A>B -> true;
        A<B -> false
    end.

compare(A,B,Field) when is_atom(Field) or is_tuple(Field) ->
    compare(A,B,[Field]);
compare(_A,_B,[]) ->
    %% IF it gets this far, then it's equal, so let's return true
    true;
compare(A,B,[SortField|RestSorts]) ->
    {SortFun,Field} = case SortField of
        {'<',F} -> {fun less_equal/2,F};
        {'>',F} -> {fun greater_equal/2,F};
        {asc,F} -> {fun less_equal/2,F};
        {desc,F}-> {fun greater_equal/2,F};
        {Fun,F} when is_function(Fun) -> {Fun,F};
        F -> {fun less_equal/2,F}
    end,

    Ax = get(A,Field),
    Bx = get(B,Field),

    case SortFun(Ax,Bx) of
        equal -> compare(A,B,RestSorts);
        less -> true;
        true -> true;
        greater -> false;
        false -> false
    end.

% this converts Obj to a proplist, runs the script as the proplist, and returns
% it back to it's original type
-spec as_list(object(), fun()) -> object().
%as_list(Obj, Fun) when is_list(Obj) ->
%    Fun(Obj);
as_list(Obj, Fun) when is_map(Obj) ->
    from_and_to_map(Obj, Fun);
as_list(Obj, Fun) ->
    from_and_to_this_type(Obj, Fun).

-spec to_list(object()) -> proplist().
to_list(Obj) when is_list(Obj) ->
    Obj;
to_list(Obj) when is_map(Obj) ->
    maps:to_list(Obj);
to_list(Obj) ->
    ?MOD(Obj):to_list(Obj).

-spec to_map(object()) -> map().
to_map(Obj) when is_list(Obj) ->
    maps:from_list(Obj);
to_map(Obj) when is_map(Obj) ->
    Obj;
to_map(Obj) ->
    List = to_list(Obj),
    maps:from_list(List).

-spec to_type(obj_type(), object()) -> object().
to_type(list, Obj) ->
    to_list(Obj);
to_type(map, Obj) ->
    to_map(Obj);
to_type(Type, Obj) ->
    Mod = get_type_handler_from_type_name(Type),
    List = to_list(Obj),
    Mod:from_list(List).

%% Only uses the expansion mods
from_and_to_this_type(Obj, Fun) ->
    Mod = ?MOD(Obj),
    List = Mod:to_list(Obj),
    List2 = Fun(List),
    Mod:from_list(List2).

from_and_to_map(Obj, Fun) ->
    List = maps:to_list(Obj),
    List2 = Fun(List),
    maps:from_list(List2).

-spec type(object()) -> obj_type().
type(Obj) when is_list(Obj) ->
    list;
type(Obj) when is_map(Obj) ->
    map;
type(Obj) ->
    Mod = get_type_handler(Obj),
    Mod:type().

get_type_handler(Obj) ->
    Mods = erlang_ds_register:get_type_handlers(),
    get_type_handler(Obj, Mods).

get_type_handler(Obj, [Mod|Mods]) ->
    case Mod:is_type(Obj) of
        true -> Mod;
        false -> get_type_handler(Obj, Mods)
    end;
get_type_handler(Obj, []) ->
    error({erlang_ds, {no_type_handler_for_this_object, Obj}}).

get_type_handler_from_type_name(Type) ->
    Mods = erlang_ds_register:get_type_handlers(),
    get_type_handler_from_type_name(Type, Mods).

get_type_handler_from_type_name(Type, [H|T]) ->
    case H:type()==Type of
        true -> H;
        false -> get_type_handler_from_type_name(Type, T)
    end;
get_type_handler_from_type_name(Type, []) ->
    error({unknown_type, Type}).


-include_lib("eunit/include/eunit.hrl").

base_pl() ->
    [{a,1},{b,2},{c,3}].

base_map() ->
    maps:from_list(base_pl()).

base_dict() ->
    dict:from_list(base_pl()).

pl_test_() ->
    Obj = base_pl(),
    obj_test_core(Obj).

map_test_() ->
    Obj = base_map(),
    obj_test_core(Obj).

dict_test_() ->
    ds:register_type_handler(erlang_ds_dict),
    Obj = base_dict(),
    obj_test_core(Obj) ++ dict_test_core(Obj).

obj_test_core(Obj) ->
    [   
        ?_assert(has_key(Obj, b)),
        ?_assertNot(has_key(Obj, x)),
        ?_assertEqual(1, get(Obj, a)),
        ?_assertEqual(2, get(Obj, b)),
        ?_assertEqual(3, get(Obj, c)),
        ?_assertEqual(4, get(set(Obj, b, 4), b)),
        ?_assertEqual([3,1,2], get_list(Obj, [c,a,b])),
        ?_assertEqual("", get(Obj, x)),
        ?_assertEqual(4, get(set(Obj, d, 4), d)),
        ?_assertEqual(123, get(Obj, x, 123)),
        ?_assertEqual("", get(delete(Obj, a), a)),
        ?_assertEqual(["", "", 3], get_list(delete_list(Obj, [a,b]), [a,b,c])),
        ?_assertEqual([1, "", 3], get_list(keep(Obj, [a,c]), [a,b,c])),
        ?_assertEqual("", get(delete(Obj, x), x)), %% just showing it doesn't crash
        ?_assertEqual([1,2,3,10,11], get_list(set(Obj, [{j,10},{k,11}]), [a,b,c,j,k])),
        ?_assertEqual(["","","",1,2,3], get_list(rekey(Obj, [{a, x}, {b, y}, {c, z}]), [a,b,c,x,y,z])),
        ?_assertEqual(4, get(update(Obj, b, fun(X) -> X*X end), b)), %% 2*2 = 4, duh,
        %% multiplifing the default values by themslves
        ?_assertEqual([1,4,9], get_list(transform(Obj, [{fun(X) -> X*X end, [a,b,c]}]), [a,b,c])),
        ?_assertEqual(list, type(to_list(Obj))),
        ?_assertEqual(map, type(to_map(Obj)))
    ].

dict_test_core(Obj) ->
    [
        ?_assertEqual(dict, type(to_type(dict, Obj)))
    ].
