-module(erlang_ds_qdate_updaters).
-dialyzer(no_unknown).
-export([
    register_updaters/0,
    unregister_updaters/0
]).

-export([
    to_date/1,
    to_unixtime/1,
    to_now/1,
    to_string/2
]).

to_date(D) ->
    try qdate:to_date(D)
    catch _:_ -> {{1970,1,1},{0,0,0}}
    end.

to_unixtime(D) ->
    try qdate:to_unixtime(D)
    catch _:_ -> 0
    end.

to_now(D) ->
    try qdate:to_unixtime(D)
    catch _:_ -> {0,0,0}
    end.

to_string(Format, D) ->
    try qdate:to_string(Format, D)
    catch _:_ -> "Invalid Date or Date Format"
    end.

updaters() -> 
    [
        {date, {?MODULE, to_date}},
        {unixtime, {?MODULE, to_unixtime}},
        {now, {?MODULE, to_now}},
        {{date, 1}, {?MODULE, to_string, 2}}
    ].

register_updaters() ->
    ds:register_updaters(updaters()).

unregister_updaters() ->
    Keys = proplists:get_keys(updaters()),
    ds:unregister_updaters(Keys).
    
