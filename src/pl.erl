%% pl: Proplist Helper lib for 
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
	rekey/2,
	rekey/3,
	keep/2,
%	qry/2,
	boolize/2,
	atomize/2,
	transform/2,
	compare/3,
	merge/1,
	merge/2,
	guess_merge/1,
	guess_merge/2
	]).

% Can set with set(Proplist,Key,Val) or Set(PropList,{Key,Val}) or Set(Proplist,[{Key,Val},{Key,Val}])
% retrieve values with get(Proplist,Key) or get(Proplist,Key,IfNotFound)
% delete keys with delete(Proplist,Key)


set(PL,Key,Val) ->
	set(PL,{Key,Val}).

set(PL,{Key,Val}) ->
	% delete a matching key if exists and prepend {K,V} to the list. No guarantee of proplist order
	[{Key,Val} | delete(PL,Key)];
set(PL,[]) ->
	PL;
set(PL,[{Key,Val} | Rest]) ->
	NewPL = set(PL,{Key,Val}),
	set(NewPL,Rest).

% Will get a list of values in the order specified in the "Keys" argument
get_list(PL,Keys,Default) when is_list(Keys) ->
	[get(PL,Key,Default) || Key<-Keys].

get_list(PL,Keys) when is_list(Keys) ->
	get_list(PL,Keys,"").

% Get a single value using the key "Key" and if not found, return "Default"
get(PL,Key,Default) ->
	proplists:get_value(Key,PL,Default).

get(PL,Key) ->
	get(PL,Key,"").

has_key(PL,Key) ->
	lists:keymember(Key,1,PL).

update(PL,[],_FormatFun) ->
	PL;
update(PL,[Key|RestKeys],FormatFun) ->
	NewPL = update(PL,Key,FormatFun),
	update(NewPL,RestKeys,FormatFun);
update(PL,Key,FormatFun) ->
	NewVal = FormatFun(get(PL,Key)),
	set(PL,Key,NewVal).


boolize(PL,Keys) ->
	update(PL,Keys,fun(V) ->
		if
			V==0;
			V=="0";
			V==undefined;
			V=="" -> false;
			true -> true
		end
	end).

atomize(PL,Keys) ->
	update(PL,Keys,fun
		(V) when is_list(V) -> list_to_atom(V);
		(V) when is_atom(V) -> V;
		(V) when is_binary(V) -> list_to_atom(binary_to_list(V));
		(V) when is_integer(V) -> list_to_atom(integer_to_list(V));
		(V) -> throw({cannot_convert_to_atom,V})
	end).

transform(PL,{boolize,Keys}) ->
	boolize(PL,Keys);
transform(PL,{atomize,Keys}) ->
	atomize(PL,Keys);
transform(PL,{Fun,Keys}) when is_function(Fun) ->
	update(PL,Keys,Fun);

transform(PL,Map) when is_list(Map) ->
	lists:foldl(fun(Action,Acc) ->
		transform(Acc,Action)
	end,PL,Map).
	

map(PL,Fun) ->
	[{Key,Fun(Val)} || {Key,Val} <- PL].	

rekey(PL,FromKey,ToKey) ->
	rekey(PL,[{FromKey,ToKey}]).

rekey(PL,KeyMap) ->
	lists:map(fun({K,V}) ->
		{rekey_swapkey(K,KeyMap),V}
	end,PL).
		

rekey_swapkey(Current,KeyMap) ->
	%% Since the KeyMap is itself a proplist of {oldkey,newkey},
	%% let's find oldkey and return it's "value", and if it's not found, just
	%% return itself
	get(KeyMap,Current,Current).		


% TODO: improve performance by only traversing main list once. Fast enough for now
delete_list(PL,[Key|RestKeys]) ->
	NewPL = delete(PL,Key),
	delete_list(NewPL,RestKeys).

delete(PL,Key) ->
	lists:keydelete(Key, 1, PL).


% Opposite of delete(): keeps all listed keys and removes all others
keep(PL,Keys) ->
	FilterFun = fun({Key,_Val}) ->
		lists:member(Key,Keys)
	end,
	
	lists:filter(FilterFun,PL).

merge_full(ExtendedProplist) ->
	lists:foldl(fun
		({_,Ignore},Acc) when Ignore=="" orelse Ignore==undefined ->
				%% Ignore all values of ""
				Acc;
		({Key,Val},{Merged,Unmerged}) ->
			case get(Unmerged,Key,undefined) of
				undefined ->
					%% This key is not flagged as an unmergable one, so we try it out
					case get(Merged,Key) of
						NewV when NewV=="" orelse NewV==undefined -> 
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
	merge_full(lists:flatten(List)).

%% Returns a single proplist with a guess of the final proplist.
%% For each field, just returns the first value it encounters, even if it's not the same
guess_merge(List) ->
	FinalDict = lists:foldl(fun({K,V},Dict) ->
		case not(dict:is_key(K,Dict)) 
					andalso V=/=undefined 
					andalso has_non_whitespace(V)  of
			true -> dict:store(K,V,Dict);
			false -> Dict
		end
	end,dict:new(),lists:flatten(List)),
	dict:to_list(FinalDict).

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
%qry(PLL,{SearchKey,SearchVal}) ->
%	[].

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


