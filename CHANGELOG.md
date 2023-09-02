# Erlang DS Change Log

## 0.2.2

* Update `set/2` such that the second argument can be another object (not just
  a key-value-list). Semantically, this makes it similar to `guess_merge/2`,
  with the argument order flipped.

## 0.2.1

* Fix the hex listing so its not depending on qdate
* Move changelog into its own file
* Tweak readme

## 0.2.0

* Add type handler system (define your own types).
* Move `dict` from natively being handled by `ds` to a separate type handler
  (that is loaded and enabled by default).
* Add custom updater registration system (define your own convenience
  shortcuts for `update/3` and `transform/2`).
* Move the qdate functionality into its own module as custom updaters. You can
  enable the qdate functionality with: `ds:register_qdate_updaters()` and
  `ds:unregister_qdate_updaters()`. By default, the qdate functionality is not
  loaded/enabled.
* Move `atomize` and `boolize` out of the core functionality and added as
  pre-built (and pre-loaded) updaters.  Remove `ds:atomize` and `ds:boolize` as functions. If
  you need them, use `ds:update(Obj, Keys, atomize)` or `ds:update(OBj, Keys,
  boolize)`.
* Now properly passing dialyzer tests.

## 0.1.1

* Add support for `map` using fun with arity 2

## 0.1.0

* First version ready for public consumption.
* Complete conversion from `sigma_proplist`.
* Add support for maps and dict natively.
* Add tests and type specs.
