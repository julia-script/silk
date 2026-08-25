## Author Approval

On 2026-08-24, the human author explicitly approved this bounded language change to proceed directly
to OpenSpec without an SLP. This change therefore carries the design evidence, alternatives,
whole-language interaction map, privilege boundary, and normative scenarios that an SLP handoff
would otherwise provide.

## Why

Silk can now parse enum declarations, but they have no semantic identity, values, representation,
operators, matching behavior, or runtime realization. Programs that need a small closed set of named
states must currently use unrelated integers, losing nominal type safety and exhaustive matching, or
use payload-bearing structural unions, which model a different problem and hide their runtime tags.

Scalar enums fill that gap with one explicit model: a closed nominal type whose fieldless members are
values and whose physical representation is a fixed-width integer chosen by the declaration.

## What Changes

- Add closed, nominal, fieldless scalar enums. `AssertionResult.Pass` constructs a value of
  `AssertionResult`; it is not a call and allocates nothing.
- Make omitted representation exactly `u8`. Explicit representations are the fixed-width integer
  types `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, and `i64`.
- Assign discriminants in declaration order: the first implicit value is `0`; each later implicit
  value is its predecessor plus one. Explicit discriminants are signed decimal integer literals.
- Reject empty enums, duplicate member names, duplicate discriminants, unsupported representations,
  signedness mismatches, and explicit or implicit overflow.
- Give enums the exact size, alignment, and calling shape of their representation integer, while
  preserving nominal type identity. Every enum is `Copy` and has no cleanup obligation.
- Expose enum-to-integer conversion as `EnumName.value(enumValue)`, returning the exact representation
  type. Add no integer-to-enum conversion.
- Support `==` and `!=` only between values of the same enum type. Require `.value` before numeric
  ordering or integer comparison.
- Extend matching with qualified member patterns such as `AssertionResult.Pass`, exhaustive coverage
  over the enum's closed member set, `_` for remaining members, and deterministic unreachable-arm
  diagnostics.
- Carry enum identity and member identity through analysis, HIR, MIR, evaluation, Wasm, and native
  lowering. Physical lowering may use the representation lane but may not admit undeclared values.

## Capabilities

### New Capability

- `bootstrap-scalar-enums`: the complete source-visible scalar-enum model, including declarations,
  identity, discriminants, conversion, diagnostics, matching, ownership, and representation.

### Modified Capabilities

- `bootstrap-lexer`, `bootstrap-syntax`: reserve and parse lossless, recoverable enum declarations,
  member access, and enum-member patterns.
- `bootstrap-intrinsic-boundary`: expose only the target-neutral backing-value primitive needed by the
  enum declaration's generated `value` wrapper.
- `bootstrap-declaration-index`, `bootstrap-name-resolution`: collect nominal enum identities and
  resolve qualified members through those identities and visibility boundaries.
- `bootstrap-operator-semantics`, `bootstrap-exhaustive-matching`: define homogeneous equality and
  closed-set member coverage.
- `bootstrap-ownership`, `bootstrap-target-layout`: make enums cleanup-free `Copy` values with the
  exact layout of their declared representation.
- `bootstrap-hir`, `bootstrap-mir`: retain canonical enum and member identity while permitting scalar
  physical lowering.
- `bootstrap-evaluation`, `bootstrap-backend`: realize construction, conversion, equality, and
  matching consistently across all execution engines.
- `bootstrap-analysis-facade`: expose enum declaration and realization facts without tooling
  reconstructing semantics.
- `silk-source-formatting`: define canonical formatting for default and explicit representations and
  explicit discriminants.

Existing CodeMirror, TextMate, and docs-highlighting specifications already require parity with the
compiler token inventory, so no enum-specific duplicate delta is needed for them.

## Impact

The change touches every compiler phase from declaration collection through backend lowering, the
analysis facade, diagnostic catalog, formatter, syntax highlighting, and tests. It introduces no
standard-library declaration recognized by spelling, allocation, or hidden runtime metadata. Its one
new compiler primitive is sealed under `Intrinsic` and exists only to realize the generated public
`EnumName.value` wrapper.

The parser, formatter, and highlighting slice is already implemented on `feature/enum-syntax` in
commit `84af799`; the remaining work starts at declaration and semantic analysis and must update the
existing slice wherever the final semantic representation requires it.

Out of scope: payload-bearing enum cases, generic enums, member-level visibility, arbitrary constant
expressions as discriminants, inferred representation widths, integer-to-enum conversion, enum
ordering operators, flags/bitsets, custom enum representations, and changing structural-union tag
visibility.
