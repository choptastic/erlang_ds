# Data Structure Agnostic Library for Erlang

Introducing a simple library tailored for Erlang's common key-value data
structures.  Currently it ships supporting proplists, maps, and dicts (with a
built-in handler). It can be further expanded to support new types with the
handler system.  With a universal interface, seamlessly convert between
structures and harness additional functionalities not natively found in some,
or even any, of these data types. Just "do the damn thing" without sweating
about the underlying structure.

Contemplating an upgrade for a legacy app from proplists to maps? This tool's
got your back. With `ds`, maintain consistent calls for a smooth, hassle-free
transition.


## Key Points to Remember:

* The central module is `ds`. For instance, to fetch values, use `ds:get`.
* Calls are designed for brevity. Hence, `ds:get` is favored over
  `proplist:get_value`. The choice of using `ds` with the `D` and `S` keys
  adjacent to eachother is fully intentional for typing speed.
* The supported structures are
  [maps](https://www.erlang.org/doc/man/maps.html),
  and `{Key,Value}` tuple
  lists, the latter generally referred in this document as
  [proplists](https://www.erlang.org/doc/man/proplists.html), although
  `{Key,Value}` tuple lists are technically only a subset of proplists.
  [dicts](https://www.erlang.org/doc/man/dict.html) are also supported with the
  use of handler module that's shipped with `erlang_ds`.
* Calls are structure-agnostic with the supported types - meaning the calls
  will work regardless of the data structure type (as long as the type is
  supported).
* Functions that return a modified version of the provided data, such as
  `ds:set(Obj, Key, Value)`, ensure the returned structure matches the input.
  For example, if `Obj` is a map, the output is also a map.
* Typically, `Obj` (the data structure we're working with) is the
  firstargument, contrasting the common Erlang practice of making it the last
  argument.
* In most cases, when a `Default` value exists, it's positioned as the
  function's last argument. E.g., `ds:get(Obj, Key, DefaultValue)`.
* **Discussion Point**: The default return for missing values is either an
  empty string (`""`) or an empty list (`[]`). If this isn't favorable, we're
  open to making it configurable.

## Function Reference

**Reference Preface 1:** When we mention "object" or `Obj`, we're referring to
the provided data structure, even if it doesn't align with the traditional OO
paradigm.

**Reference Preface 2:** Optional Arguments are denoted by `[Brackets]`

### Getting, Setting, and Deleting (one value)

* `ds:get(Obj, Key, [Default])`: Retrieve the value associated with `Key` in
  `Obj`.  If missing, `Default` is returned. Without `Default`, it returns
  `""`.
* `ds:set(Obj, Key, Value)`: Sets `Key` in `Obj` to `Value` and returns the
  updated object.

### Getting and Setting (multiple values)

* `ds:get_list(Obj, ListOfKeys, [Default])`: Fetches values associated with
  each key in `ListOfKeys`. If a value is missing, it returns either the
  `Default` or `""` for that value. Returns a list of values in the same order
  as the keys in `ListOfKeys`.  Example: `[A,B] = ds:get_list(Obj, [a,b])`
* `ds:set(Obj, ListOfKeyValues)`: Here, `ListOfKeyValues` is a set of
  `{Key,Value}` tuples. This function updates `Obj` in bulk with these values.

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

* `ds:map(Obj, Fun)` - Run `Fun` on every entry in `Obj` and set each entry's
  new value to the return value from `Fun`. `Fun` can be defined as either
  `Fun(Value) -> NewValue` or `Fun(Key, Value) -> NewValue`, either way, the
  value of each entry will be set to `NewValue`.
* `ds:update(Obj, ListOfKeys, Updater)` - Update the values in `Obj` associated
  with each `Key` in `ListOfKeys` by running each associated value through the
  provided `Updater`. `Updater` can be a function with arity-1 (e.g.
  `Fun(Value)`), or it can be an atom or a tuple of the form
  `{UpdaterName, Arg1, Arg2...}`. For example, to convert a handful of values
  to their integer forms (or `undefined` if not parseable), you could implement
  it like this: `ds:update(Obj, ListOfKeys, fun(X) -> try list_to_integer(X)
  catch _:_ -> undefined end)`.  Two default built-in updaters are `atomize`
  and `boolize`, which will convert terms to an atom or a boolean,
  respectively.

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

* `ds:type(Obj)` - returns the type of data structure (`map`, `list`, `dict`, etc).
* `ds:to_list(Obj)` - Convert `Obj` to a proplist. If `Obj` is already a
  list, it returns it unchanged.
* `ds:to_map(Obj)`: Convert `Obj` to a map. If `Obj` is already a map,
  returns it unchanged.
* `ds:to_type(Type, Obj)`: Convert `Obj` to the provided `Type`.

### Comparison Helpers for Sorting lists of Objects

* `ds:compare(ObjA, ObjB, SortCriteria)` - `SortCriteria` can be a single
  `Key` or `{Operation, Key}` tuple, or it can be a list of `Key` or
  `{Operation, Key}` tuples. `Operation` can be the atoms `<` or `asc` (for
  ascending)  or the atoms `>` or `desc` (for descending). `Operation` can
  also be a function of arity 2 (meaning, it takes 2 arguments). These
  arguments will be the values associated with `Key` of from `ObjA` and
  `ObjB`. If no `Operation` is provided (that is, if an item in
  `SortCriteria` is not a 2-tuple, it's treated as a `Key` and `asc` is used
  (sort in ascending order).

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

## Expanding `erlang_ds` with new updaters

You can create your own custom updaters to be used with `ds:update/3` and
`ds:transform/2`.

To register a custom updater, you call can take 2 possible forms.

### The full custom updater

The more powerful updater format is this:

`ds:register({UpdaterName, NumUpdaterArgs}, {Module, Function, NumArgs})`

### Example

If you regularly update values to be formatted as strings with something like
this:

```erlang
FormatFun = fun(D) -> qdate:to_string("Y-m-d", D) end,
ds:update(Obj, [signup_date, valid_date], FormatFun).
```

You could register a default formatter like this:

```erlang
ds:register_updater({format_date, 1}, {qdate, to_string, 2}).
```

Once that's done, you can do this call anywhere in your code:

```erlang
ds:update(Obj, [signup_date, valid_date], {format_date, "Y-m-d"}).
```

This is the same as calling `qdate:to_string("Y-m-d", X)` on the provided
fields.

### The simple custom updater

If you have a basic updater you might use regularly (one that doesn't take
additional arguments), you can use the short syntax for this.

In the above case, let's say you also very regularly convert your values
specifically to ISO-formatted strings, you could make a short version. Let's
start by defining a basic module here:

```erlang
-module(my_formatter).
-export([format_iso/1]).

format_iso(D) ->
	qdate:to_string("Y-m-d", D).
```

Now you can register this function with the shorter
`ds:register_updater(UpdaterName, {Module, Function})`:

```erlang
ds:register_updater(iso_date, {my_formatter, format_iso}).
```

And you can then simply call this those fields to ISO dates.
```erlang
ds:update(Obj, [signup_date, valid_date], iso_date).
```

The observant reader may have noticed that the following are identical:

```erlang
ds:register_updater(UpdaterName, {Module, Function}).

ds:register_updater({UpdaterName, 0}, {Module, Function, 1}).`
```

Worth noting is that `UpdaterArgs` must *always* be one less than the
`FunctionArgs`.

## Expanding `erlang_ds` with new types

Extending `erlang_ds` to support new data types is quite easy.

Create a new module, add the attribute `-behavior(erlang_ds_type_handler).`

And define the following functions:

* `type() -> atom()`
* `is_type(Obj) -> boolean()`
* `set(Obj, Key, Val) -> NewObj`
* `get(Obj, Key, DefaultValue) -> Val`
* `has_key(Obj, Key) -> boolean()`
* `delete(Obj, Key) -> NewObj`
* `filter(Obj, Fun(K, V)) -> NewObject`
* `to_list(Obj) -> Proplist`
* `from_list(Proplist) -> Obj`

(see `erlang_ds_dict.erl` for an example).

## Add to your rebar.config's deps section

```erlang
{deps, [
    erlang_ds
]}.
```

## Origins & Philosophy

Erlang DS emerged from the need for a unified module with concise function
calls tailored for proplists, especially before Erlang's introduction of the
map structure. Although maps have become a preferred choice, legacy code still
utilizes proplists or dicts, or even a mix of the trio.

For a few examples of the motivation to create this:

* While `maps` and `dict` both support merging, `proplists` does not.
* While `ds:transform` and `ds:rekey` can both be implemented with
  `Module:map`, `map` is clunky for both.
* While `map` supports getting multiple things per line with pattern matching
  (`#{key:=Value, key2:=Value2} = Map`), this is not supported by proplists or
  dicts. Also, the map matching syntax above will crash if one of the values
  isn't present, necessitating the `maps:get/3` function anyway.
* The inconsistencies of between the data structure modules makes me constantly
  forget which module does it which way.  `get` and `set`, for example, which
  are the most common operations in any of my codebases (and I suspect the most
  common in yours as well) are implemented in `maps` as `get` and `put`, in
  `dict` as `find` and `store`, in `proplists` as `get_value` and *not
  implemented* (basically, just delete the old value and prepend the new
  `{Key,Value}` tuple), or with the `lists` module as `keyfind` and `keystore`.

None of these comments is to criticize the Erlang team - they are incredible,
and what they've built is incredibly powerful. But no one can be everything to
everyone, and tailor their development to every one of their users'
requirements or idiocyncracies. Hence, this library was born to bridge such
gaps and offer utility.

Constructive feedback and pull requests are heartily welcomed.

**PULL REQUESTS ARE WELCOMED**

## Additional Notes

Formerly known as `sigma_proplist`, Erlang DS originated from [Sigma Star
Systems](https://sigma-star.com). After nearly a decade in production, it was
refashioned to support more data structures, resulting in the Erlang DS we have
today.

## Versions

### 0.2.0

* Add type handler system (define your own types)
* Move `dict` from natively being handled by `ds` to a separate `handler`.
* Add custom updater registration system (define your own pre-built convenience
  shortcuts for `update/3` and `transform/2`)
* Now properly passing dialyzer tests

### 0.1.1

* Add support for `map` using fun with arity 2

### 0.1.0

* First version recommended for the public
* Complete conversion from `sigma_proplist`
* Add support for maps and dict natively.
* Add tests and type specs

## About

Author: [Jesse Gumm](https://jessegumm.com)

Copyright 2013-2023

MIT License
