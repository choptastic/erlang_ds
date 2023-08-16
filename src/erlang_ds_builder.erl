-module(erlang_ds_builder).
-export([build/0]).

build() ->
    Updaters = application:get_env(erlang_ds, updaters, []),
    TypeModules = application:get_env(erlang_ds, type_modules, []),
    logger:notice("Erlang DS: Generating and building erlang_ds_lookup..."),
    {Time, _} = timer:tc(fun() -> build(Updaters, TypeModules) end),
    logger:notice("Erlang: DS: erlang_ds_lookup built in ~p ms", [Time div 1000]).


build(Updaters, TypeModules) ->
    Header = header(),    
    UpdaterBody = updater_body(Updaters),
    TypeBody = type_body(TypeModules),

    Modtext = lists:flatten([Header, "\n", UpdaterBody, "\n", TypeBody]),
    logger:warning("Module:~n~s",[Modtext]),
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

build_updater_clause({Key, {Mod, Fun, 0}}) ->
    io_lib:format("updater(~p) -> fun(Val) -> ~p:~p(Val) end;\n", [Key, Mod, Fun]);

build_updater_clause({Key, {Mod, Fun, NumArgs}}) ->
    Args0 = lists:map(fun(X) ->
        "Arg" ++ integer_to_list(X)
    end, lists:seq(1, NumArgs)),
    Args = string:join(Args0, ", "),
    io_lib:format("updater({~p, ~s}) -> fun(Val) -> ~p:~p(~s, Val) end;\n", [Key, Args, Mod, Fun, Args]).


