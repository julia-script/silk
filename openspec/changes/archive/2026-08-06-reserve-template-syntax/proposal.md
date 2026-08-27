## Why

Silk intends to support unprefixed JSX-like template expressions in the future, so primary-expression syntax beginning with `<` must not acquire an incompatible meaning before that work starts. Fixed-array source types currently occupy unrelated angle-bracket syntax; changing them now, while the language is still in alpha, creates a clearer type grammar without carrying a compatibility burden into the eventual template design.

## What Changes

- **BREAKING** Replace fixed-array source types from `Array<T, N>` to `[T; N]`, including recursively nested forms such as `[[I32; 4]; 3]`; the old spelling is rejected rather than supported as an alias.
- Add `;` as punctuation used between the element type and decimal length of a fixed-array source type.
- Reserve `<Tag ...>`, self-closing `<Tag ... />`, and fragment `<>...</>` starts at primary-expression boundaries for a future built-in template language. Until that language is implemented, these forms remain unsupported and recover as reserved template syntax.
- Preserve `<`, `<=`, `>`, and `>=` as relational operators when they continue an already-started Silk expression.
- Canonically format fixed-array source types as `[T; N]` while preserving their lossless syntax and local recovery guarantees.
- Do not introduce template AST, semantic, HIR, MIR, runtime, component, attribute, child, whitespace, escaping, or lowering behavior in this change.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-syntax`: Change fixed-array source grammar and recovery to `[T; N]`, and reserve JSX-like template starts specifically at primary-expression boundaries without changing relational parsing.
- `silk-source-formatting`: Define the canonical formatted spelling and spacing for bracketed fixed-array source types.

## Impact

- Affects compiler tokenization, concrete syntax, parser recovery, formatting, syntax inspection, source fixtures, and parser/formatter tests under `packages/compiler` and `packages/editor-support`.
- Requires source examples and syntax-focused OpenSpec scenarios that currently spell `Array<T, N>` to adopt `[T; N]`.
- Leaves the structural fixed-array semantic type, type equality, ownership, layout, HIR, MIR, and backend behavior unchanged; internal semantic encodings need not adopt source syntax merely to reserve the grammar.
- Adds no runtime dependency and does not implement or select a template runtime protocol.
