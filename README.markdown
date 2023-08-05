# Erlang Generized Data Structure Library

*Everyone has to reinvent this wheel, here's my version*

A simple library for doing common things with common Erlang data structures (proplists, dicts, and maps for now). all with the same function calls. These all follow certain conventions:

  * Calls are short (`ds:get` instead of `proplists:get_value` or `lists:keyfind`).
  * Perform inline actions with some standardized procedures for common things (e.g. `ds:atomize(Obj, [key1, key2])` converts the values of the associated keys `key1` and `key2` into atoms and returns the new Object
  * Filtering Keys (`ds:keep(Obj, [good_key, other_good_key])` removes all keys from the Object that aren't missing. The reverse of this is `ds:delete_list(Obj, [bad_key, other_bad_key])`
  * Merging objects
  * Provides a helper for sorting lists of objects

More documentation to come

## Add to your rebar.config's deps section

```erlang
{deps, [
	erlang_ds
]}.
```

## About

Author: [Jesse Gumm](http://github.com/choptastic)

MIT License
