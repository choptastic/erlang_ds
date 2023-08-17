-module(ds_util).

-export([
    to_bool/1,
    to_atom/1,
    normalize_updater_key/1,
    normalize_updater_pair/2,
    normalize_updaters/1
]).

to_bool(V) ->
    if
        V=="false";
        V==<<"false">>;
        V==false;
        V==0;
        V=="0";
        V==undefined;
        V=="" -> false;
        V == <<>> -> false;
        true -> true
    end.

to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_binary(V) -> list_to_atom(binary_to_list(V));
to_atom(V) when is_integer(V) -> list_to_atom(integer_to_list(V));
to_atom(V) -> throw({cannot_convert_to_atom,V}).

normalize_updater_key(Key) when is_atom(Key) ->
    {Key, 0};
normalize_updater_key({Key, Int}) when is_atom(Key), is_integer(Int), Int>=0 ->
    {Key, Int};
normalize_updater_key(Key) ->
    error({invalid_updater_key, Key}).

normalize_updaters(Updaters) ->
    lists:map(fun({Key, MFA}) ->
        ds_util:normalize_updater_pair(Key, MFA)
    end, Updaters).

normalize_updater_pair({Key, 0}, {Mod, Fun}) when is_atom(Key), is_atom(Mod), is_atom(Fun) ->
    {{Key, 0}, {Mod, Fun, 1}};
normalize_updater_pair(Key, {Mod, Fun, 1}) when is_atom(Key), is_atom(Mod), is_atom(Fun) ->
    {{Key, 0}, {Mod, Fun, 1}};
normalize_updater_pair(Key, {Mod, Fun}) when is_atom(Key), is_atom(Mod), is_atom(Fun) ->
    {{Key, 0}, {Mod, Fun, 1}};

normalize_updater_pair(Key={KeyTag, KeyArgs}, MFA = {Mod, Fun, Args})
  when is_atom(KeyTag), is_atom(Mod), is_atom(Fun), is_integer(Args), KeyArgs >=0, KeyArgs==Args-1 ->
    {Key, MFA};
normalize_updater_pair({Key, KeyArgs}, MFA = {_, _, Args}) when KeyArgs =/= Args -1 ->
    Reason = fmt("KeyArgs (~p) needs to be exactly 1 less than Args (~p) in the MFA tuple", [KeyArgs, Args]),
    invalid_update_pair(Key, MFA, Reason);
normalize_updater_pair(Key, MFA={Mod, _, _}) when not(is_atom(Mod)) ->
    Reason = fmt("Mod (~p) must be an atom", [Mod]),
    invalid_update_pair(Key, MFA, Reason);
normalize_updater_pair(Key, MFA={_, Fun, _}) when not(is_atom(Fun)) ->
    Reason = fmt("Fun (~p) must be an atom", [Fun]),
    invalid_update_pair(Key, MFA, Reason);
normalize_updater_pair(Key, MFA={_, _, Args}) when not(is_integer(Args)) ->
    Reason = fmt("Args (element 3 from the MFA tuple) (~p) must be an integer greater than or equal to 1", [Args]),
    invalid_update_pair(Key, MFA, Reason);
normalize_updater_pair(Key={KeyTag, _}, MFA) when not(is_atom(KeyTag)) ->
    Reason = fmt("KeyTag (~p) must be an atom", [KeyTag]),
    invalid_update_pair(Key, MFA, Reason);
normalize_updater_pair(Key={_, KeyArgs}, MFA) when not(is_integer(KeyArgs)) ->
    Reason = fmt("KeyArgs (~p) must be an integer", [KeyArgs]),
    invalid_update_pair(Key, MFA, Reason);
normalize_updater_pair(Key={_KeyTag, KeyArgs}, MFA={Mod, Fun}) ->
    Reason = fmt("If the KeyArgs (~p) part of the Key (~p) is greater than 0, then MFA (~p) must be provided explicitly as a 3-tuple, specifying the arity (number of arguments) of the function call. Expected MFA value here is: {~p, ~p, ~p}", [KeyArgs, Key, MFA, Mod, Fun, KeyArgs+1]),
    invalid_update_pair(Key, MFA, Reason);
normalize_updater_pair(Key, MFA) ->
    Reason = "There is something wrong with the formatting of your MFA or your Key. Please review the specs for register_updater/2",
    invalid_update_pair(Key, MFA, Reason).

invalid_update_pair(Key, MFA, Reason) ->
    error({invalid_key_and_mfa_combo, [{key, Key}, {mfa, MFA}, {description, Reason}]}).

fmt(Msg, Args) ->
    lists:flatten(io_lib:format(Msg, Args)).
