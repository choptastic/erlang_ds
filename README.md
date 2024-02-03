# Data Structure Agnostic Key-Value Library for Erlang

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

## Important Links

* [Announcement on the Erlang Forums](https://erlangforums.com/t/2813)
* [Github](https://github.com/choptastic/erlang_ds)
* [Hex](https://hex.pm/packages/erlang_ds)

## Key Points to Remember

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

## Modified Syntax Plugin (*experimental*/optional)

Erlang DS comes with an optional modification to the Erlang syntax to satisfy
those who thrive on brevity, while also making the `get` syntax more familiar
to non-Erlang developers.

With this syntax plugin enabled, the following calls are equivalent:

```erlang
X = Obj->key.
X = ds:get(Obj, key).
```

And these two calls are equivalent:

```erlang
[A,B,C] = Obj->[key1, key2, key3].
[A,B,C] = ds:get_list(Obj, [key1, key2, key3]).
```

For more information, see the "Adding the Syntax Plugin" section at the bottom
of this page. Please note this functionality is *experimental*.

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
* `ds:foreach(Obj, Fun)` similar to `ds:map/2`, but since it only traverses
  the object to maybe generate a side effect for each element, the resulting
  list is discarded and only return `ok`.
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

#### Built-In Special Updaters

There are a few built-in custom updaters that can be enabled or disabled (see
"Expanding `erlang_ds` with custom updaters for how to make your own custom
updaters).

**These updaters are enabled by default:**

* `atomize` - This will convert the values to an atom. **Note:** this uses
  `list_to_atom` and so if used indiscriminately, can lead to filling up the
  atom table and crashing your node).
* `boolize` - This will convert the values to a boolean. **Note:** This is
  highly accepting of various terms that might *mean* false.  For example, the
  following terms all evaluate to false: `0`, `"false"`, `""` (empty string or
  empty list), `<<>>`, `"0"`, `undefined`.  Basically, if the term is zero,
  empty, the word "false", or the atom `undefined`, it will evaluate to
  `false`. All others evaluate to `true`. If this loose definition of `boolize`
  is not acceptable to you, you are free to register your own (replacing an
  updater with a new one is perfectly fine).

**These qdate-based updaters are NOT enabled by default**

* `date` - This converts the value to a `{date(), time()}` tuple.
* `now` - This converts the value to an Erlang Timestamp in the `now()` format
  (`{Megasecs, Seconds, Microsecs}`).
* `unixtime` - This converts the value to a Unix timestamp (integer).
* `{date, StringFormat}` - This converts to the provided value to the specified
  date format as used by `qdate` (which follows the conventions of PHP's
  `date()` function)

### Transforming: Updating on Steroids

* `ds:transform(Obj, TransformList)` - Run many different updates on `Obj`.
  `TransformList` is a list of `{Operation, ListOfKeys}` tuples. `Operation`
  can be a function that takes a single `Value` and returns a new `Value`, or
  `Operation` can be any valid term used as an `Updater` in `ds:update/3`.
  Returns the `Obj` with all the updates applied.

  As an example of why transforming is useful, you might have a function that
  gets a value from the database, then does something to each field to format
  it:

  ```erlang
  %% get all the fields for a certain record from the person table
  Rec = db:map("select * from person where personid=?",[ID]).
  
  ds:transform(Rec, [
    {atomize, [status]},
    {{date, "Y-m-d"}, [date_of_birth, registration_date, expiration_date]},
    {boolize, [is_active]},
    {fun my_util:decrypt/1, [encrypted_data]}
  ]).
  ```

  You can see with the above, and a relatively few lines of code, we've taken a
  record from the database, and formatted it to be something useful:
  * converted the `status` field to an atom
  * changed a handful of fields to a date format we want to use
  * converted the `is_active` field to a boolean, and
  * decrypted some data that we stored in an encrypted format in the database.

  And because you can use a combination of custom updaters and anonymous function
  calls, you can see how this will help with productivity, as `ds` becomes a part
  of your coding patterns.

### Conversion and Type-Checking

* `ds:type(Obj)` - returns the type of data structure (`map`, `list`, `dict`,
  etc).
* `ds:to_list(Obj)` - Convert `Obj` to a proplist. If `Obj` is already a
  list, it returns it unchanged.
* `ds:to_map(Obj)`-  Convert `Obj` to a map. If `Obj` is already a map,
  returns it unchanged.
* `ds:to_type(Type, Obj)` - Convert `Obj` to the provided `Type`.

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

## Expanding `erlang_ds` with Custom Updaters

You can create your own custom updaters to be used with `ds:update/3` and
`ds:transform/2`.

To register a custom updater, you call can take 2 possible forms.

### The Full Custom Updater

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

### The Simple Custom Updater

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
(see [`erlang_ds_type_handler.erl`](https://github.com/choptastic/erlang_ds/blob/master/src/erlang_ds_type_handler.erl)
for behavior callback details).

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

([see `erlang_ds_dict.erl` for an example](https://github.com/choptastic/erlang_ds/blob/master/src/erlang_ds_dict.erl)).

## Add to your rebar.config's deps section

```erlang
{deps, [
    erlang_ds
]}.
```

## Adding the SyntPlugin


If you want to add Erlang DS's syntax customizations to your app, doing so is very easy:

1. Add the following line to your somewhere above your function definitions:

   ```erlang
   -compile({parse_transform, ds_syntax}).
   ```

2. Add `erlang_ds` to the `plugins` section in to your `rebar.config`:

   ```erlang
   {plugins, [
      erlang_ds
   ]}.
   ```
   
3. Add the provider hook to your `rebar.config`:

   ```erlang
   {provider_hooks, [
      {pre, [
         {compile, {ds_syntax, arrow}}
      ]}
   ]}.
   ```
   Please note that the `arrow` mentioned above is in reference to the use of
   `->`.  Some other variants may be added in the future, depending on user
   interest.

4. Recompile your code:

   ```bash
   rebar3 compile
   ```

5. Profit?

### Experimental!

This syntax plugin is still experimental, and I probably don't need to say it, but language purists will not like this.

But if you do decide to use it, here are some important points to note:

* The syntax for retrieving a single value is: `Obj->Key`. `Key` can  be a
  Variable, String, Binary, Tuple, or an expression wrapped in parentheses.
* The syntax for retrieving a list of values is: `Obj->[Key1, Key2, ...]`, in
  this case, `KeyX` can be any expression.
* In *both* expressions above `Obj` must be a variable. If `Obj` is anything
  but a variable, it will not trigger the tokenizer and you'll be presented
  with an error calling out an illegally placed `->`.
* You may have noticed that the decision to use `get` or `get_list` is the
  presence of a bracket (`[`) immediately after an arrow (`->`). if you want to retrieve
  a value where its key is something like `[a,b,c]`, you'll need to bind it to
  a variable first.  For example: `Obj->[a,b,c]` is equivalent to
  `ds:get_list(Obj, [a,b,c]])`, but `Key=[a,b,c], Obj->Key` is equivalent to
  `Key=[a,b,c], ds:get(Obj, Key)`.
* A `string`, however, (despite being internally represented as a list of
  integers) does not trigger this difference because the Erlang scanner
  specifically tokenizes strings as their own thing. As a result: `Obj->"some
  string key"` is perfectly acceptable, and translates to `ds:get(Obj, "some
  string key")`.

### Examples

Here are a handful of examples (with the equivalent `ds` calls)

```erlang
Obj->a,         % ds:get(Obj, a)
Obj->A,         % ds:get(Obj, A)
Obj->{a,b},     % ds:get(Obj, {a,b}),
Obj->"key",     % ds:get(Obj, "key"),
(Obj)->a,       % error: left-hand-side of -> must be a variable
(#{})->a,       % error left-hand-side of -> must be a variable
Obj->f(),       % ds:get(Obj, f)()
                %   more readable version: Fun = ds:get(Obj, f),
                                           Fun().
Obj->(f()),     % ds:get(Obj, f())
Obj->(m:f()),   % ds:get(Obj, m:f())
Obj->m:f(),     % ds:get(Obj, m):f()  probably not what you intend.
Obj->a->b,      % error. Will translate to ds:get(Obj, a)->b, then will
                %   throw an error because the left-hand-side of -> is
                %   not a variable.

Obj->[a],       % ds:get_list(Obj, [a]),
Obj->[a,b],     % ds:get_list(Obj, [a,b]),
Obj->[m:f()],   % ds:get_list(Obj, [m:f()]),
Obj->["key"],   % ds:get_list(Obj, ["key"]),
Obj->[[a,b]],   % ds:get_list(Obj, [[a,b]]).
Obj->([a,b]).   % ds:get(Obj, [a,b]), %% notice the () around the list tells
                %   the parser that you're getting a single value
```

### How does the syntax plugin work?

The parse transform powering it isn't actually a parse transform.  Instead,
the plugin hijacks the parser and does an initial pass over the tokens looking
for a number of specific patterns. The `parse_transform` itself just indicates
to the parser then to parse it with the `ds_syntax` plugin.

### Known Limitations of the syntax plugin

Aside from the restrictions above, a current known limitation is that there is
not currently any support for setting values with the syntax plugin. This will
likely be changed in the near future, but currently, there is no `setting`
mechanism for it.

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

[See Changelog](https://github.com/choptastic/erlang_ds/blob/master/CHANGELOG.md)

## About

Author: [Jesse Gumm](https://jessegumm.com)

Copyright 2013-2024, Jesse Gumm

[MIT License](https://github.com/choptastic/erlang_ds/blob/master/LICENSE.md)
