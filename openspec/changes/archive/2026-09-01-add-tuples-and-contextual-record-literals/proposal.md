## Why

Silk needs compact heterogeneous argument bundles and locally inferred aggregate values without
introducing structural typing or forcing callers to import a nominal parameter type that is already
known from a function signature. Recording tuples and contextual record literals now also gives
future static reflection and formatting work a stable aggregate model to build on.

## What Changes

- Add nominal positional declarations such as `tuple Point(u32, u32)` and positional construction
  through `Point(0, 0)`.
- Add positional tuple literals such as `(0, 0)`, which construct an expected named tuple when one
  exists and otherwise receive one stable anonymous nominal identity at that literal occurrence.
- Add `.{ name: "Julia", age: 32 }` record literals, which construct an expected named struct when
  one exists and otherwise receive one stable anonymous nominal struct identity.
- Permit a locally inferred anonymous aggregate to flow through bindings and generic calls while
  preventing separate same-shaped literals from acquiring structural compatibility, branch
  unification, or cross-occurrence equality.
- Represent named tuples, anonymous tuples, and anonymous records through the existing struct type,
  value, HIR, instance, ownership, layout, and cleanup machinery rather than adding a parallel
  aggregate runtime model.
- Keep tuple positions distinct from record labels: labeled tuples and shape-based struct
  compatibility are not introduced.
- Reserve `tuple` as a declaration keyword and tuple/record literal punctuation as expression
  syntax. This is a source-breaking lexical change for any existing declaration named `tuple`.
- Record static reflection and formatting as downstream consumers; this change does not add field
  iteration, compile-time reflection, variadic calls, or formatting APIs.

## Capabilities

### New Capabilities

- `tuple-and-contextual-record-literals`: Defines named and anonymous aggregate identity,
  contextual construction, inference boundaries, and the absence of structural compatibility.

### Modified Capabilities

- `bootstrap-syntax`: Adds lossless tuple declarations, positional tuple literals, and contextual
  record literals with bounded recovery.
- `bootstrap-struct-types`: Admits compiler-synthesized nominal struct declarations for anonymous
  aggregates and positional named tuple declarations.
- `bootstrap-struct-values`: Resolves contextual literals to expected nominal aggregates or creates
  a source-stable anonymous nominal aggregate when no expected type exists.
- `bootstrap-hir`: Erases all tuple and contextual-record sugar into canonical nominal struct
  construction and projection facts.
- `bootstrap-semantic-facts`: Exposes contextual targets, generated aggregate identities, and
  source-to-canonical field or position mappings.
- `bootstrap-instances`: Carries generated nominal aggregate identities through generic instance
  keys and runtime aggregate reachability.
- `bootstrap-ownership`: Applies ordinary whole-struct moves, Copy evidence, and cleanup to every
  named or anonymous aggregate introduced by this syntax.
- `bootstrap-exhaustive-matching`: Excludes distinct occurrence-generated anonymous aggregates
  from the ordinary nominal match-result union rule unless an enclosing named context resolves
  every arm to that same nominal type.

## Impact

The change affects the lexer, parser, syntax encodings, declaration indexing, expected-type
analysis, aggregate inference, semantic facts, HIR elaboration, instance discovery, ownership,
layout consumers, diagnostics, inspectors, formatting, and the language reference. It introduces
no new runtime representation, backend aggregate category, compiler-known standard-library actor,
or compatibility path. Future formatting and static-reflection changes may consume these aggregate
facts but remain separate capabilities.
