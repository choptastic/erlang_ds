%% Copyright Jesse Gumm 2023
%% MIT License
-module(pl).

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
    to_dict/1,
    to_map/1
    ]).

% Can set with set(Proplist,Key,Val) or Set(PropList,{Key,Val}) or Set(Proplist,[{Key,Val},{Key,Val}])
% retrieve values with get(Proplist,Key) or get(Proplist,Key,IfNotFound)
% delete keys with delete(Proplist,Key)

-define(IS_DICT(V), (is_tuple(V) andalso element(1, V)==dict)).
-define(IS_BLANK(V), (V==undefined orelse V=="" orelse V==<<>>)).
-define(IS_PROPLIST(V), (is_list(V) andalso is_tuple(hd(V)) andalso tuple_size(hd(V))==2)).

set(Obj,Key,Val) ->
    set(Obj,{Key,Val}).

set(Obj,{Key,Val}) when is_list(Obj) ->
    % delete a matching key if exists and prepend {K,V} to the list. No guarantee of proplist order
    [{Key,Val} | delete(Obj,Key)];
set(Obj,{Key,Val}) when is_map(Obj) ->
    maps:set(Key, Val, Obj);
set(Obj,{Key,Val}) when ?IS_DICT(Obj) ->
    dict:store(Key, Val, Obj);
set(Obj,[]) ->
    Obj;
set(Obj,[{Key,Val} | Rest]) ->
    NewObj = set(Obj,{Key,Val}),
    set(NewObj,Rest).

% Will get a list of values in the order specified in the "Keys" argument
get_list(Obj,Keys,Default) when is_list(Keys) ->
    [get(Obj,Key,Default) || Key<-Keys].

get_list(Obj,Keys) when is_list(Keys) ->
    get_list(Obj,Keys,"").

% Get a single value using the key "Key" and if not found, return "Default"
get(Obj,Key,Default) when is_list(Obj) ->
    proplists:get_value(Key,Obj,Default);
get(Obj,Key,Default) when is_map(Obj) ->
    maps:get(Key,Obj,Default);
get(Obj,Key,Default) when ?IS_DICT(Obj) ->
    case dict:find(Key, Obj) of
        {ok, Val} -> Val;
        error -> Default
    end.

get(Obj,Key) ->
    get(Obj,Key,"").

has_key(Obj,Key) when is_list(Obj) ->
    lists:keymember(Key,1,Obj);
has_key(Obj,Key) when is_map(Obj) ->
    maps:is_key(Key, Obj);
has_key(Obj,Key) when ?IS_DICT(Obj) ->
    dict:is_key(Key, Obj).

update(Obj,[],_FormatFun) ->
    Obj;
update(Obj,[Key|RestKeys],FormatFun) ->
    NewObj = update(Obj,Key,FormatFun),
    update(NewObj,RestKeys,FormatFun);
update(Obj,Key,FormatFun) ->
    NewVal = FormatFun(get(Obj,Key)),
    set(Obj,Key,NewVal).


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
format_date(Obj, Keys, DateFormat) ->
    UpdateFun = case DateFormat of
        unixtime -> fun(D) -> try qdate:to_unixtime(D) catch _:_ -> 0 end end;
        date -> fun(D) -> try qdate:to_date(D) catch _:_ -> {{1970,1,1},{0,0,0}} end end;
        now -> fun(D) -> try qdate:to_now(D) catch _:_ -> {0,0,0} end end;
        Format -> fun(D) -> try qdate:to_string(Format, D) catch _:_ -> "Invalid Format" end end
    end,
    update(Obj, Keys, UpdateFun).

atomize(Obj,Keys) ->
    update(Obj,Keys,fun
        (V) when is_list(V) -> list_to_atom(V);
        (V) when is_atom(V) -> V;
        (V) when is_binary(V) -> list_to_atom(binary_to_list(V));
        (V) when is_integer(V) -> list_to_atom(integer_to_list(V));
        (V) -> throw({cannot_convert_to_atom,V})
    end).


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

map(Obj,Fun) when is_list(Obj) ->
    [{Key,Fun(Val)} || {Key,Val} <- Obj];
map(Obj,Fun) when is_map(Obj); ?IS_DICT(Obj) ->
    as_list(Obj, fun(List) -> map(List, Fun) end).

rekey(Obj,FromKey,ToKey) ->
    rekey(Obj,[{FromKey,ToKey}]).

rekey(Obj,KeyMap) when is_list(Obj) ->
    lists:map(fun({K,V}) ->
        {rekey_swapkey(K,KeyMap),V}
    end,Obj);
rekey(Obj,KeyMap) when is_map(Obj); ?IS_DICT(Obj) ->
    as_list(Obj, fun(List) -> rekey(List, KeyMap) end);

rekey_swapkey(Current,KeyMap) ->
    %% Since the KeyMap is itself a proplist of {oldkey,newkey},
    %% let's find oldkey and return it's "value", and if it's not found, just
    %% return itself
    get(KeyMap,Current,Current).


% TODO: improve performance by only traversing main list once. Fast enough for now
delete_list(Obj, []) ->
    Obj;
delete_list(Obj,[Key|RestKeys]) ->
    NewObj = delete(Obj,Key),
    delete_list(NewObj,RestKeys).

delete(Obj,Key) when is_list(Obj) ->
    lists:keydelete(Key, 1, Obj);
delete(Obj,Key) when is_map(Obj) ->
    maps:remove(Key, Obj);
delete(Obj,Key) when ?IS_DICT(Obj) ->
    dict:erase(Key, Obj).


% Opposite of delete_list(): keeps all listed keys and removes all others
keep(Obj,Keys) ->
    FilterFun = fun({Key,_Val}) ->
        lists:member(Key,Keys)
    end,
    Filter(Obj, FilterFun).

filter(Obj, Fun) when is_list(Obj) ->
    lists:filter(FilterFun,Obj);
filter(Obj, Fun) when is_map(Obj) ->
    maps:filter(FilterFun,Obj);
filter(Obj, Fun) when ?IS_DICT(Obj) ->
    dict:filter(FilterFun, Obj).
    

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
    
merge(A,B) ->
    merge([A,B]).
    
%% Returns a two_tuple: {Merged, Unmerged}
%% Merged is the new merged result
merge(List) ->
    Type = type(hd(List))
    List2 = lists:flatten([to_list(X) || X <- List]),
    {Merged, Unmerged} = merge_full(lists:flatten(List)),
    {to_type(Type, Merged), to_type(Type, Unmerged)}.

%% Returns a single proplist with a guess of the final proplist.
%% For each field, just returns the first value it encounters, even if it's not the same
guess_merge([]) ->
    [];
guess_merge(List) when ?IS_PROPLIST(List)
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
as_list(Fun, Obj) when is_list(Obj) ->
    Fun(Obj);
as_list(Fun, Obj) when is_map(Fun) ->
    from_and_to_map(Fun, Obj);
as_list(Fun, Obj) when ?IS_DICT(Fun) ->
    from_and_to_dict(Fun, Obj).

to_list(Obj) when is_list(Obj) ->
    Obj;
to_list(Obj) when is_map(Obj) ->
    maps:to_list(Obj);
to_list(Obj) when ?IS_DICT(Obj) ->
    dict:to_list(Obj).

to_dict(Obj) when ?IS_DICT(Obj) ->
    Obj;
to_dict(Obj) when is_list(Obj) ->
    dict:from_list(Obj);
to_dict(Obj) when is_map(Obj) ->
    List = maps:to_list(List),
    dict:from_list(Obj).

to_map(Obj) when is_list(Obj) ->
    maps:from_list(Obj);
to_map(Obj) when is_map(Obj) ->
    Obj;
to_map(Obj) when ?IS_DICT(Obj) ->
    List = to_list(Obj),
    maps:from_list(List).


to_type(list, Obj) ->
    to_list(Obj);
to_type(map, Obj) ->
    to_map(Obj);
to_type(dict, Obj) ->
    to_dict(Obj).

from_and_to_type(Fun, Obj) ->
    Type = type(Obj),
    from_and_to_type(Type, Fun, Obj).


from_and_to_type(list, Fun, Obj) ->
    Fun(Obj);
from_and_to_type(map, Fun, Obj) ->
    from_and_to_map(Fun, Obj);
from_and_to_type(dict, Fun, Obj) ->
    from_and_to_dict(Fun, Obj).

from_and_to_dict(Fun, Obj) ->
    List = dict:to_list(Obj),
    List2 = Fun(List),
    dict:from_list(Obj).

from_and_to_map(Fun, Obj) ->
    List = maps:to_list(Obj),
    List2 = Fun(List),
    maps:from_list(Obj).

type(Obj) when is_list(Obj) ->
    list;
type(Obj) when is_map(Obj) ->
    map;
type(Obj) when ?IS_DICT(Obj) ->
    dict.



