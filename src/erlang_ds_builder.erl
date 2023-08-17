-module(erlang_ds_builder).
-export([
    build/0,
    default_updaters/0,
    default_type_modules/0
]).

default_updaters() ->
    [
        {boolize, {ds_util, to_bool}},
        {atomize, {ds_util, to_atom}}
    ].

default_type_modules() ->
    [
        erlang_ds_dict
    ].

build() ->
    Updaters0 = case application:get_env(erlang_ds, updaters) of
        {ok, U} ->
            U;
        undefined ->
            U = default_updaters(),    
            logger:notice("Erlang DS: No updaters defined in the application config.~nUsing the default: ~p",[U]),
            U
    end,

    TypeModules0 = case application:get_env(erlang_ds, type_handlers) of
        {ok, T} ->
            verify_type_handlers(T);
        undefined ->
            T = default_type_modules(),
            logger:notice("Erlang DS: No type_handlers defined in the application config.~nUsing the default: ~p",[T]),
            T
    end,
    Updaters = ds_util:normalize_updaters(Updaters0),
    TypeModules = verify_type_handlers(TypeModules0),
    logger:notice("Erlang DS: Generating and building erlang_ds_lookup..."),
    {Time, _} = timer:tc(fun() -> build(Updaters, TypeModules) end),
    logger:notice("Erlang: DS: erlang_ds_lookup built in ~p ms", [Time div 1000]).

verify_type_handlers(Ts) ->
    Good = lists:all(fun(T) -> is_atom(T) end, Ts),
    case Good of 
        true -> Ts;
        false ->
            error({erlang_ds, {invalid_type_handlers_from_config, Ts}})
    end.


build(Updaters, TypeModules) ->
    Header = header(),    
    UpdaterBody = updater_body(Updaters),
    TypeBody = type_body(TypeModules),

    Modtext = lists:flatten([Header, "\n", UpdaterBody, "\n", TypeBody]),
    logger:info("Erlang DS Generated Module:~n~s",[Modtext]),
    Forms = merl:quote(Modtext),
    Res = merl:compile_and_load(Forms),
    case Res of
        {ok, _} -> ok;
        error ->
            logger:error("Erlang DS: Unable to compile the lookup module (erlang_ds_lookup).~n~s~n",[Modtext])
    end.

header() ->
    "-module(erlang_ds_lookup).
    -export([
        updater/1,
        type_handlers/0
    ]).\n".

type_body(Mods) ->
    io_lib:format("type_handlers() -> ~p.", [Mods]).

updater_body([]) ->
    "updater(Updater) -> error({erlang_ds_lookup, {unregistered_updater, Updater}}).\n";
updater_body([H|T]) ->
    UpdaterClause = build_updater_clause(H),
    [UpdaterClause | updater_body(T)].

build_updater_clause({{Key, 0}, {Mod, Fun, 1}}) ->
    io_lib:format("updater(~p) -> fun(Val) -> ~p:~p(Val) end;\n", [Key, Mod, Fun]);

build_updater_clause({{Key, KeyArgs}, {Mod, Fun, NumArgs}}) when KeyArgs==NumArgs-1 ->
    Args0 = lists:map(fun(X) ->
        "Arg" ++ integer_to_list(X)
    end, lists:seq(1, KeyArgs)),
    Args = string:join(Args0, ", "),
    io_lib:format("updater({~p, ~s}) -> fun(Val) -> ~p:~p(~s, Val) end;\n", [Key, Args, Mod, Fun, Args]).

