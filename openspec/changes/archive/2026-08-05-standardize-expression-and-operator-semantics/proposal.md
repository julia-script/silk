## Why

Silk's bootstrap compiler can execute arithmetic and comparisons, but source programs must spell
them as qualified intrinsic calls and the parser has no general precedence, grouping, or operator
model. Standardizing expressions now gives structs, arrays, and loops one coherent surface while
preserving Wayfinder's rule that behavior remains qualified, data-first actor operations rather
than method lookup or overload sets.

## What Changes

- Add lossless tokens and concrete syntax for grouping; prefix `-` and `!`; infix `*`, `/`, `%`,
  `+`, `-`, `<`, `<=`, `>`, `>=`, `==`, and `!=`; and the accepted `|>` pipeline operator.
- Define a fixed bootstrap precedence table and associativity: grouping/primary, prefix,
  multiplicative, additive, relational, equality, then left-associative pipeline.
- Resolve arithmetic and relational operators to the existing compiler-known `I32` actor
  operations, boolean negation to `Bool.not`, and equality to the compiler-known operation for the
  operands' identical scalar type. No implicit numeric conversions, truthiness, user-defined
  operators, or import-dependent overload search are introduced.
- Elaborate operators and pipelines into ordinary canonical HIR operations and calls. A pipeline
  inserts its left value as argument zero of an explicit qualified call; it does not create a
  runtime pipe object or a separate backend instruction.
- Preserve exact source provenance, parser recovery, deterministic encodings, arithmetic trap
  behavior, and interpreter/native/WebAssembly parity for operator-authored programs.
- Extend facade-only labs with precedence, grouping, operator-resolution, pipeline, recovery, and
  trap presets.
- **BREAKING**: expression parsing gains standardized precedence, so previously unsupported token
  sequences now have a defined tree and meaning; negative non-literal expressions are represented
  as prefix operations rather than malformed signed literals.

## Capabilities

### New Capabilities

- `bootstrap-operator-semantics`: Defines the closed bootstrap operator vocabulary, precedence,
  associativity, actor-operation mapping, type rules, pipeline insertion, and deferrals.

### Modified Capabilities

- `bootstrap-lexer`: Recognizes every operator delimiter as deterministic lossless tokens.
- `bootstrap-syntax`: Parses grouped, prefix, infix, and pipeline expressions with bounded recovery.
- `bootstrap-semantic-facts`: Publishes resolved operator and pipeline relationships, types, and
  unavailable states.
- `bootstrap-hir`: Represents operator-authored expressions as the same canonical builtin and call
  operations used by qualified source calls.
- `bootstrap-syntax-inspector`: Exposes precedence, resolution, provenance, lowering, and runtime
  outcomes through facade-only labs.

## Impact

The compiler lexer, concrete syntax vocabulary, parser, elaboration facts, HIR construction and
encoders, ownership traversal, lowering fixtures, interpreter and backend corpora, and docs labs are
affected. The public compiler data model grows new token/node/fact variants, which is allowed during
alpha. MIR and backend operation vocabularies remain backend-agnostic and are reused rather than
forked for surface syntax.
