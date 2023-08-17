-module(erlang_ds_register).

-dialyzer([
    {nowarn_function, bench/0},
    {nowarn_function, get_type_handlers_appvar/0}
]).

-export([
    get_type_handlers/0,
    register_type_handler/1,
    unregister_type_handler/1
]).

-export([
    get_updater/1,
    register_updater/2,
    unregister_updater/1
]).

-export([
    build_lookup/0
]).

%% only used for testing
-export([
    bench/0
]).

get_type_handlers() ->
    case erlang:function_exported(erlang_ds_lookup, type_handlers, 0) of
        true ->
            erlang_ds_lookup:type_handlers();
        false ->
            build_lookup(),
            erlang_ds_lookup:type_handlers()
    end.

get_type_handlers_appvar() ->
    application:get_env(erlang_ds, type_handlers, []).

%% To verify how much faster the module-based version is
%% On my machine, module-based lookup is 5-12x faster
bench() ->
    register_type_handler(a),
    register_type_handler(b),
    register_type_handler(c),

    Seq = lists:seq(1, 100000),
    {Timer1, Res1} = timer:tc(fun() ->
        [get_type_handlers() || _ <- Seq]
    end),

    {Timer2, Res2} = timer:tc(fun() ->
        [get_type_handlers_appvar() || _ <- Seq]
    end),
    io:format("PrintingResults:~n~p~nFrom Module: ~p.~nFrom App Var: ~p",[Res1 ++ Res2, Timer1, Timer2]).


register_type_handler(Mod) when is_atom(Mod) ->
    with_app_var(type_handlers, erlang_ds_builder:default_type_modules(), fun(Mods) ->
        case lists:member(Mod, Mods) of
            true ->
                Mods;
            false ->
                [Mod | Mods]
        end
    end),
    build_lookup().

unregister_type_handler(Mod) when is_atom(Mod) ->
    with_app_var(type_handlers, erlang_ds_builder:default_type_modules(), fun(Mods) ->
        Mods -- [Mod]
    end),
    build_lookup().



get_updater(UpdaterKey) ->
    case erlang:function_exported(erlang_ds_lookup, updater, 1) of
        true ->
            erlang_ds_lookup:updater(UpdaterKey);
        false ->
            build_lookup(),
            erlang_ds_lookup:updater(UpdaterKey)
    end.


register_updater(Key0, MFA0) ->
    {Key, MFA} = ds_util:normalize_updater_pair(Key0, MFA0),
    store_updater(Key, MFA).
      
store_updater(Key, MFA) ->
    with_app_var(updaters, erlang_ds_builder:default_updaters() , fun(Updaters) ->
        ds:set(Updaters, Key, MFA)
    end),
    build_lookup().

unregister_updater(Key0) ->
    Key = ds_util:normalize_updater_key(Key0),
    with_app_var(updaters, erlang_ds_builder:default_updaters(), fun(Updaters) ->
        ds:delete(Updaters, Key)
    end),
    build_lookup().


build_lookup() ->
    erlang_ds_builder:build().

with_app_var(Key, Default, Fun) ->
    Var = application:get_env(erlang_ds, Key, Default),
    NewVar = Fun(Var),
    application:set_env(erlang_ds, Key, NewVar).
