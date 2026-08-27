# `@silk-lang/doctest`

Compiles the fenced Silk examples carried by Silk Effect documentation JSON, and reports each
failure with the file and the line the example was written on.

`silk doc` emits a formatter-neutral documentation JSON value. Every fenced block in a `///` or
`//!` comment reaches that value as a code block with its language token and a module-relative byte
range. This package reads those, compiles them, and turns the byte range back into a position an
author can open.

## Running it

```console
$ silk-doctest --stdlib
Doctests: 2 collected, 1 passed, 1 skipped, 0 failed.
```

```console
$ silk build && silk doc --output build/documentation.json
$ silk-doctest --input build/documentation.json --source-root src
```

`--source-root` is only used to turn a byte offset into a line. Without it an example still
compiles and still fails; its position is reported as unavailable rather than guessed.

The exit status is `1` when an example failed, `2` when the input could not be read, and `0`
otherwise.

## What an example is

An example is compiled as **one complete module, exactly as written**. Nothing is prepended, nothing
wraps it, and it is never concatenated with the module that documents it:

````silk
/// Returns the present value, or the fallback value when the option is absent.
///
/// # Examples
///
/// ```silk
/// import silk.option { none, some, unwrapOr }
///
/// pub fn main() -> i32 {
///   let present = some<i32>(7)
///   let absent = none<i32>()
///   return unwrapOr<i32>(move present, 0) + unwrapOr<i32>(move absent, 5)
/// }
/// ```
pub fn unwrapOr<T>(self: Option<T>, fallback: T) -> T {
````

Silk has no implicit prelude, and no single entry shape that would be right for both a pure fragment
and one that runs effects, so there is no wrapper that could be synthesized without deciding a
language question. A fragment therefore opts out instead of being guessed at.

## Opting out

A form that is meant to be read rather than compiled is fenced with a comma:

````silk
/// ```silk,ignore
/// let scope = run temporaryDirectory(&parent, "silk-build-")
/// let value = run Effect.ensuring(build(&artifact), releaseIgnored(move scope))
/// ```
````

**The comma is required.** The prose documents under `packages/language/docs/` spell a skipped
example ```` ```silk ignore ````, and that form does not work inside a documentation comment:
CommonMark splits a fence's info string into a language word and a trailing meta string, and the
documentation model records only the language word. By the time the JSON is written,
```` ```silk ignore ```` and ```` ```silk ```` are the same value. A comma is not whitespace, so
```` ```silk,ignore ```` survives.

An attribute this package does not define fails the example rather than being discarded, so a
misspelled marker cannot quietly turn an opted-out example back into a compiled one.

## Reading the report

```
silk/option:48: example in silk/option::unwrapOr failed
  SEM0004: Unknown function unwrapOrElse
  | import silk.option { none, some, unwrapOr }
  |
  | pub fn main() -> i32 {
  ...
```

The position is the fence's own line in the module that documents it, not a line inside the
extracted example.

## Modules

- `Example` — collects fenced Silk examples from a documentation value and parses fence attributes.
- `Doctest` — compiles them and reports the outcome of each.
- `Report` — renders a run as text.
- `Sources` — turns a module-relative byte offset into a one-based line.
- `Stdlib` — documentation for the compiler-shipped standard library, and its source lookup.
