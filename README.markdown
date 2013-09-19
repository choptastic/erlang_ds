# Sigma Proplist

*Everyone has to reinvent this wheel, here's my version*

A simple library for doing common things with Erlang proplists following certain conventions:

  * Calls are short (`pl:get` instead of `proplists:get_value` or `lists:keyfind`).
  * Perform inline actions with some standardized procedures for common things (e.g. `pl:atomize(PL, [key1, key2])` converts the values of the associated keys `key1` and `key2` into atoms and returns the new Proplist.
  * Filtering Keys (`pl:keep(PL, [good_key, other_good_key])` removes all keys from the proplist that aren't missing. The reverse of this is `pl:delete_list(PL, [bad_key, other_bad_key])`
  * Merging Proplists
  * Provides a helper for sorting lists of proplists

More documentation to come

## Add to your app with rebar

```erlang
{deps, [
	{sigma_proplist, ".*", {git, "git://github.com/choptastic/sigma_proplist.git", {branch, master}}}
]}.
```

## About

Author: [Jesse Gumm](http://github.com/choptastic) ([@jessegumm](http://twitter.com/jessegumm))

MIT License
