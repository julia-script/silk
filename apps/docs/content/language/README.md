# Silk language documentation

Silk is a low-level language with explicit ownership, lazy typed Effects, replaceable service
requirements, deterministic cleanup, and target-aware compilation. This documentation describes
the language that the current alpha compiler implements.

## Start here

- **[Getting started](./tutorial.md)** — create a project and learn the core language by writing
  functions, loops, owned values, borrows, unions, Effects, typed failures, and a service provider.
- **[Language reference](./reference.md)** — look up the exact lexical, type, ownership, Effect,
  declaration, and control-flow rules.
- **[Alpha status](./alpha-status.md)** — see what is implemented, which targets are supported, and
  which compatibility promises the alpha does not make.

## Understand the model

- **[Ownership, borrowing, and cleanup](./ownership.md)** — learn when a value copies, when it
  moves, how call-scoped borrows work, and when cleanup runs.
- **[Effects, failures, and services](./effects.md)** — understand the success, failure, and
  requirement channels; run computations; recover errors; and provide capabilities lexically.
- **[Fibers and local scheduling](./fibers.md)** — run cooperative child work with an explicit
  single-threaded scheduler and structured cancellation.
- **[Recursion and stack safety](./recursion.md)** — choose between ordinary recursion, explicit
  Effect suspension, and iterative traversal.

## Look things up

- **[Standard library](./stdlib/)** — every shipped module and public declaration, generated from
  the Silk source doc comments.
- **[Diagnostic index](./diagnostics.md)** — every stable compiler error code and its message
  shape.

The standard-library and diagnostic pages are generated. Regenerate them with
`pnpm --filter @silk-lang/compiler documentation:generate`. The compiler test suite rejects stale
generated pages and compiles every active Silk example in the hand-written pages.

## Deeper rule evidence

The compact reference is written for day-to-day use. The app also publishes a
[prescriptive rule-by-rule reference](../reference/) with status, boundaries, diagnostics, and
links to the tests and specifications behind each programmer-visible rule. It governs intended
language behavior and is also useful when contributing to the compiler or reconciling an edge case.

The editor-support package is documented separately in the
[package README](https://github.com/julia-script/silk/blob/main/packages/language/README.md). Its
CodeMirror and TextMate APIs are not part of the Silk language.
