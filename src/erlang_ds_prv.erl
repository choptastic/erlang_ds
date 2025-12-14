-module(erlang_ds_prv).
-dialyzer(no_undefined_callbacks).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, arrow).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {namespace, ds_syntax},
            {bare, true},               % The task can be run by the user, always true
            {deps, ?DEPS},              % The list of dependencies
            {example, "rebar3 ds_syntax arrow"}, % How to use the plugin
            {opts, []},                  % list of options understood by the plugin
            {short_desc, "Erlang DS Syntax Helper"},
            {desc, "This adds Var->key syntax to your Erlang code that translates to `ds:get` calls."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ds_syntax:init(), 
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

