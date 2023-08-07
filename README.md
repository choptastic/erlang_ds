# Data Structure Agnostic Library for Erlang

A simple library for doing common things with common Erlang data structures
(proplists, dicts, and maps for now). This will convert from one structure to
another, and can allow you to "just do the damn thing" without worrying about
the form of the underlying data structure.

This is also designed to help the user incrementally convert an application
from one data structure to another. For example, if you have a legacy
application that uses proplists as a primary data structure, and you want to
upgrade to using maps instead, this is a handy tool for making that conversion,
since the calls to `ds` don't have to change.


## A few things worth noting:

  * The primary module here is `ds`.  So `ds:get` is a function call to get
	values.  
  * Calls are intended to be short (less typing).  So `ds:get` instead of
	`proplist:get_value`.
  * Currently supported data structures are: maps, dicts, and lists of
	`{Key,Value}` tuples (generally referred to as proplists here, but these
	are not *exactly* proplists [0]).
  * Calls work regardless of the supported data structure.
  * If the return value is intended to be a modified version of the provided
	data structure (e.g. `ds:set(Obj, Key, Value)`), then the return value will
	be of the same type.  So from that example, if `Obj` is a map, the return
	value is a map.  If `Obj` is a dict, then the return value is a dict.
  * In *most* circumstances, the `Obj` argument (the Data Structure) is the
	first argument in a function, rather than the norm in Erlang where it is
	typically the last argument. For example: `proplists:get_value(Key, Obj)`
	vs `ds:get(Obj, Key)`.
  * Also, in *most* circumstances, if there is a `Default` value (to return if
	a key isn't found) that `Default` argument will be the last argument of the
	function. For example: `ds:get(Obj, Key, DefaultValue)`.
  * **Controversial Item**: The standard return value if something isn't found
	is an empty string/empty list (`""` or `[]`).  If this is a major sticking
	point for users, this can be changed to a config setting.

## Function Reference

**`Obj` and the term "object" will be used to represent the provided data
structure, even though I'm well aware none of these are true `Objects` in the
Object-oriented sense.**

**Optional Arguments are presented in `[Brackets]`**

### Getting, Setting, and Deleting (one value)

  * `ds:get(Obj, Key, [Default])`: Get the value associated with `Key` from
	`Obj`. Return the found value, or return `Default` if no value is found,
	and if `Default` is not provided, return `""`
  * `ds:set(Obj, Key, Value)`: In `Obj` set the value associated with `Key` to
	`Value`, and return the modified object.

### Getting and Setting (multiple values)

  * `ds:get_list(Obj, ListOfKeys, [Default])`: For each item in `ListOfKeys`
	return a the associated value (or if not found, return `Default for that
	item, or if `Default` is not provided, return `""` for that key). Example:
	`[A,B] = ds:get_list(Obj, [a,b])`
  * `ds:set(Obj, ListOfKeyValues)`: `ListOfKeyValues` is a list of
	`{Key,Value}` tuples, and perform a mass-update by setting each value
	associated with `Key` to `Value` in the provided `Obj` and return the
	modified object.


### Deleting and Filtering Values

  * `ds:delete(Obj, Key)` - Remove the entry associated with `Key` from `Obj`.
	Returns the modified object.
  * `ds:delete_list(Obj, ListOfKeys)` - Removes all the entries associated with
	each `Key` in `ListOfKeys` from `Obj`. Returns the modified object.
  * `ds:keep(Obj, ListOfKeys)` - Removes all entries from `Obj` except for
	those whose keys are listed in `ListOfKeys`. Returns the modified object.
  * `ds:filter(Obj, Fun)` - Iterates over `Obj` and retains only those entries
	for which `Fun(Key, Value)` returns `true`. Returns the modified object.

### Working with Keys

  * `ds:has_key(Obj, Key)`: Checks if `Obj` contains an entry for `Key`.
	Returns `true` if found, otherwise `false`.
  * `ds:rekey(Obj, OldKey, NewKey)`: Renames `OldKey` to `NewKey` in `Obj`. If
	`NewKey` already exists, its value is overwritten. Returns the modified
	object.
  * `ds:rekey(Obj, ListOfOldAndNewKeys)`: `ListOfOldAndNewKeys` is a list of
	`{OldKey, NewKey}` tuples. Renames each `OldKey` to its corresponding
	`NewKey`. If a `NewKey` already exists, its value is overwritten. Returns
	the modified object.

### Mass Updating

  * `ds:boolize(Obj, ListOfKeys)` - Convert the values in `Obj` associated with
	each `Key` in `ListOfKeys` to a boolean
  * `ds:atomize(Obj, ListOfKeys)` - Convert the values in `Obj` associated with
	each `Key` in `ListOfKeys` to an atom.
  * `ds:map(Obj, Fun)` - Run `Fun` on every entry in `Obj` and set each entry's
	new value to the return value from `Fun`. `Fun` can be defined as either
	`Fun(Value) -> NewValue` or `Fun(Key, Value) -> NewValue`, either way, the
	value of each entry will be set to `NewValue`.
  * `ds:update(Obj, ListOfKeys, Fun)` - Update the values in `Obj` associated
	with each `Key` in `ListOfKeys` by running each associated value through
	the provided `Fun(Value)`. For example, to convert a handful of values to
	their integer forms (or `undefined` if not parseable), you could implement
	it like this: `ds:update(Obj, ListOfKeys, fun(X) -> try list_to_integer(X)
	catch _:_ -> undefined end)`

### Transforming: Updating on Steroids

  * `ds:transform(Obj, TransformList)` - Run many different updates on `Obj`.
	`TransformList` is a list of `{Operation, ListOfKeys}` tuples. `Operation`
	can be a function that takes a single `Value` and returns a new `Value`, or
	`Operation` can be any of the following terms: `atomize`, `boolize`,
	`date`, `unixtime`, `now`, `{date, DateFormat}`. `ListOfKeys` is a list of
	keys to convert. Returns the `Obj` with all the updates applied.  Bear in
	mind, the date and time related functions all assume
	[qdate](https://github.com/choptastic/qdate)
	([@hex.pm](https://hex.pm/packages/qdate)) is installed.  `qdate`, however
	is not an automatic dependency because the rest of the module works without
	it.

### Conversion and Type-Checking

  * `ds:type(Obj)` - returns the type of data structure (`map`, `dict`, or
	`list`).
  * `ds:to_list(Obj)` - Convert `Obj` to a proplist. If `Obj` is already a
	list, it returns it unchanged.
  * `ds:to_map(Obj)`: Convert `Obj` to a map. If `Obj` is already a map,
	returns it unchanged.
  * `ds:to_dict(Obj)`: Convert `Obj` to a dict. If `Obj` is already a dict,
	returns it unchanged.

### Comparison Helpers for Sorting lists of Objects

  * `ds:compare(ObjA, ObjB, SortCriteria)` - `SortCriteria` can be a single
	`Key` or `{Operation, Key}` tuple, or it can be a list of `Key` or
	`{Operation, Key}` tuples. `Operation` can be the atoms `<` or `asc` (for
	ascending)  or the atoms `>` or `desc` (for descending). `Operation` can
	also be a function of arity 2 (meaning, it takes 2 arguments). These
	arguments will be the values associated with `Key` of from `ObjA` and
	`ObjB`. If no `Operation` is provided (that is, if an item in
	`SortCriteria` is not a 2-tuple, it's treated as a `Key` and `asc` is used
	(sort in ascending order)`

### Merging Objects

  * `ds:merge(ListOfObjects)` - Merge the list of objects together. Returns
	`{Merged, Unmerged}`.  Blank values (`""`, `<<>>`, `undefined`) will be
	replaced by any other non-blank items. If there are conflicts (where
	several objects have values for the same key), then those values will be
	assembled into a list of values and returned
  * `ds:merge(ObjA, ObjB)` - A shortcut for `ds:merge([ObjA, ObjB)])`. It just
	merges two objects.
  * `ds:guess_merge(ListOfObjects)` - This works similarly to `ds:merge` except
	that this will return a single fully-merged object. Unlike `ds:merge`, if
	this encounters a conflict, it goes with the first non-blank value
	encountered.
  * `ds:guess_merge(ObjA, ObjB)` - A shortcut for `ds:guess_merge([ObjA, ObjB)])`.
    It just merges two objects.

## Add to your rebar.config's deps section

```erlang
{deps, [
	erlang_ds
]}.
```

## History and Philosophy

This library came out of a desire to have a single module with short function
calls to work with proplists (bear in mind, this was made before Erlang had a
native `map` data structure).  In most circumstances, these days, maps are the
superior data structure these days.  But there is legacy code using proplists
or dicts for things, or a mix of all three.

On top of that, there are a lot of situations where the functionality provided
by one module is insufficient for my needs. For example:

* While `maps` and `dict` both support merging, `proplists` does not.
* While `ds:transform` and `ds:rekey` can both be implemented with
  `Module:map`, `map` is clunky for both.
* While `map` supports getting multiple things per line with pattern matching
  (`#{key:=Value, key2:=Value2} = Map`), this is not supported by proplists or
  dicts. Also, the `map` syntax above will crash if one of the values isn't
  present, necessitating the `maps:get/3` function anyway.
* The inconsistencies of the modules makes me constantly forget which module
  does it which way.  `get` and `set`, for example, which are the most common
  operations in any of my codebases (and I suspect the most common in yours as
  well) are implemented in `maps` as `get` and `put`, in `dict` as `find` and
  `store`, in `proplists` as `get_value` and *not implemented* (basically,
  just delete the old value and prepend the new `{Key,Value}` tuple), or with
  the `lists` module as `keyfind` and `keystore`.

None of this is to criticize the Erlang team - they do incredible work, and
they can't do everything for everyone, nor should every API for every data
structure be exactly the same. Indeed, the `lists:key*` functions are
individually more flexible for lists of tuples of all sizes (not just the
`{Key, Value}` tuples. But for someone using mostly `{Key, Value}` tuples,
`lists:key:*` are overkill (but are very handy to use under the hood for the
functions in Erlang DS).

Perhaps my use cases are not useful to the community at
large, but I suspect there are others out there like me who need
or want this kind of utility.

Further, I acknowledge that there likely some inefficiencies here, or methods
where I might be using a less-than ideal implementation, and for that, I'm open
to comments and pull requests to improve the software.

## Final Comment

Erlang DS was originally called `sigma_proplist` using a module called just
`pl`.  "Sigma" is just a reference to the author's software business, [Sigma
Star Systems](https://sigma-star.com).  The project was renamed to "Erlang DS"
when it added support for other data structures. The original `sigma_proplists`
has been running in production for almost 10 years before converting it to
`Erlang DS`.

## About

Author: [Jesse Gumm](https://jessegumm.com)

Copyright 2013-2023

MIT License
