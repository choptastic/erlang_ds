-module(erlang_ds_handler).


-callback type() -> ds:obj_type().

-callback is_type(ds:object()) -> boolean().

-callback set(ds:object(), ds:key_value_tuple()) -> ds:object().

-callback get(ds:object(), ds:key(), ds:default_value()) -> ds:value().

-callback has_key(ds:object(), ds:key()) -> boolean().

-callback delete(ds:object(), ds:key()) -> ds:object().

-callback filter(ds:object(), fun()) -> ds:object().

-callback to_list(ds:object()) -> ds:proplist().

-callback from_list(ds:proplist()) -> ds:object().

-callback from_and_to_this_type(ds:object(), fun()) -> ds:object().

