## Why

Borrowing is currently limited by implementation-shaped roots, while callable sections support only one omitted leading argument and reusable captures often remain borrowed until explicit drop. The stabilized rules require stable temporary/subplace identities, lexical borrow values, arbitrary leading-argument sections, and last-use loan endings shared by Effects and callables.

## What Changes

- Give temporaries and stable subplaces compiler-owned owner/place identities and carry provenance through projections, locals, calls, and returned views.
- Admit local shared and exclusive borrow bindings while preserving exclusivity, non-escape, and lexical lifetime rules.
- Generalize callable sections to every `0 < K < N` supplied trailing suffix and allow staged application.
- Apply last-invocation loan shortening to reusable callables while preserving explicit drop, consuming calls, escape, and aggregate-storage lifetimes.
- Update callable representations, ownership, evaluation, MIR, LLVM, Wasm, diagnostics, and formatter behavior together.

## Capabilities

### Modified Capabilities

- `bootstrap-ownership`: generalize owners, places, provenance, lexical loans, and delayed-value last use.
- `bootstrap-runtime-slices`: allow valid views from stable temporary and local roots.
- `bootstrap-callable-values`: represent ordered omitted-parameter lists and staged application.
- `bootstrap-flow-functions`: align callable and Effect capture lifetime calculation.
- `bootstrap-mir`: carry generalized place provenance and callable-section captures.
- `bootstrap-backend`: realize generalized sections and stable borrowed storage consistently.

## Impact

Depends on `define-copy-and-executable-ownership`. It retires the unary-only section diagnostic and the named-whole-array borrow exceptions. It does not relax exclusive borrowing, allow escaping local loans, synthesize heap allocation to extend lifetimes, bind argument holes, or reorder parameters.
