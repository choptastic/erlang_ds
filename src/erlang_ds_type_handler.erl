-module(erlang_ds_type_handler).


-callback type() -> ds:obj_type().

-callback is_type(ds:object()) -> boolean().

-callback set(ds:object(), ds:key(), ds:value()) -> ds:object().

-callback get(ds:object(), ds:key(), ds:default_value()) -> ds:value().

-callback has_key(ds:object(), ds:key()) -> boolean().

-callback delete(ds:object(), ds:key()) -> ds:object().

-callback filter(ds:object(), fun()) -> ds:object().

-callback to_list(ds:object()) -> ds:proplist().

-callback from_list(ds:proplist()) -> ds:object().

