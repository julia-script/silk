## Why

Silk's structural unions compose unrelated types well, but they cannot declare one closed nominal
sum whose variants share generic parameters and belong to the same type. Closed types such as
`Option` and `Result` therefore require wrapper structs around conventionally related structural
members, adding an artificial value layer and making variant identity, generic selection, and
exhaustive matching indirect.

Nominal unions fill that gap without changing the open composition model: `union Result<A, E>` is
one declared type with `Success` and `Failure` variants, while a payload such as
`HttpErrorCode | OutOfMemoryError` remains an ordinary structural union selected independently by
each function signature.

## What Changes

- Add nonempty nominal `union` declarations with source-ordered unit and named-field variants.
- Make variants subordinate to the instantiated parent type. `Result<A, E>.Success { value }`
  explicitly selects a variant of `Result<A, E>`; named payload fields may infer omitted parent
  arguments under the existing struct-construction rules, while expected result types do not.
- Give union fields the same declaration, visibility, generic substitution, construction, pattern,
  ownership, and cleanup rules as struct fields. Unions are affine by default and admit `Copy`,
  `Drop`, operator, and interface implementations under the same validation rules as structs.
- Treat a nominal union as one atomic member of a structural union. Structural normalization never
  flattens or merges its variants, even after generic specialization.
- Extend exhaustive matching with hierarchical coverage. A match over
  `HttpErrorCode | OutOfMemoryError` may cover `HttpErrorCode.DNSTimeout`,
  `HttpErrorCode.DNSError { ... }`, and `OutOfMemoryError {}` directly; a whole
  `HttpErrorCode value` pattern covers the remaining subtree.
- Plan an inaccessible active tag plus the selected variant's aligned payload through the existing
  target-neutral layout, ownership, HIR, MIR, evaluation, and backend pipelines. Direct inline
  recursion is rejected under the same finite-layout rule as recursive structs; indirection remains
  explicit in source.
- **BREAKING**: Replace the standard-library `Option` and `Result` wrapper structs and their detached
  structural-union member structs with direct nominal unions. Update every caller, test, fixture,
  and document in the same change, and remove the superseded encodings rather than retaining aliases
  or compatibility paths.

## Capabilities

### New Capabilities

- `bootstrap-nominal-unions`: closed nominal tagged unions, including declaration and variant
  identity, generic construction, field payloads, matching, ownership, layout, and diagnostics.

### Modified Capabilities

- `bootstrap-lexer`, `bootstrap-syntax`: reserve `union` and parse recoverable declarations,
  instantiated variant selection, construction, and patterns.
- `bootstrap-declaration-index`, `bootstrap-name-resolution`, `bootstrap-type-generics`: collect
  canonical union/variant/field facts and resolve variants through a parent application completed
  from an explicit generic prefix and constructor fields.
- `bootstrap-module-semantic-surface`, `bootstrap-semantic-facts`: encode union declarations and
  variant operations as deterministic cross-module contracts and immutable semantic facts.
- `bootstrap-intrinsic-boundary`, `bootstrap-integer-scalars`: keep checked and host primitives
  carrier-neutral while ordinary integer wrappers construct the new nominal `Option` variants.
- `bootstrap-os-file-system`, `bootstrap-standard-input`, `bootstrap-host-input`: replace raw
  Option-shaped host outcomes with affine-safe handle carriers or primitive status/count outputs while
  ordinary providers retain their public Option-using APIs.
- `bootstrap-structural-unions`, `bootstrap-exhaustive-matching`: retain nominal unions as atomic
  structural members while supporting direct hierarchical variant coverage.
- `bootstrap-ownership`, `bootstrap-target-layout`: apply struct-equivalent ownership and finite
  recursive-layout rules while planning one inaccessible tag and active payload.
- `bootstrap-nominal-callable-storage`, `bootstrap-nominal-effect-storage`: realize represented
  callable and Effect fields inside the selected variant under the existing nominal-storage rules.
- `bootstrap-hir`, `bootstrap-mir`: retain parent union, variant, specialized field, selection,
  binding, cleanup, and representation identities through verified lowering.
- `bootstrap-evaluation`, `bootstrap-backend`: construct, pass, project, match, and clean nominal
  unions consistently across evaluation, direct Wasm, and native LLVM execution.
- `bootstrap-analysis-facade`: expose immutable union declaration, variant, field, coverage, layout,
  and lowering facts to tooling.
- `bootstrap-flow-functions`, `bootstrap-silk-stdlib`: reify Effect outcomes through the direct
  nominal `Result` representation, redefine `Option` and `Result` as nominal unions, and remove their
  wrapper and detached-member representations.
- `silk-source-formatting`: define canonical formatting for union declarations, unit and field
  variants, instantiated variant paths, constructors, and patterns.

## Impact

The change crosses the complete language pipeline: tokens and CST nodes, formatting and recovery,
declaration facts and module surfaces, name and generic resolution, construction and pattern
analysis, hierarchical coverage, ownership and cleanup, HIR/MIR, layout verification, evaluation,
both backends, analysis projections, diagnostics, the standard library, reference documentation,
and acceptance tests.

No compiler-known standard-library actor, source-callable intrinsic, raw union, C layout, external
linkage, tuple variant, public runtime tag, explicit discriminant, or automatic nominal-union `Copy`
derivation is introduced. Scalar `enum` remains the separate fixed-width, fieldless enumeration
construct, and `A | B` remains the open structural-union construct.
