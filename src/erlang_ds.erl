-module(erlang_ds).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% initialize all commands here
    {ok, State2} = erlang_ds_prv:init(State),
    {ok, State2}.
