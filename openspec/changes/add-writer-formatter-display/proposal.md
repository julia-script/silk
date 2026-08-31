## Why

Silk's integer formatting currently allocates an owned `String`, which makes presentation depend on
the allocator and prevents generic values from streaming directly through the existing `Writer`
service. The language also cannot currently select a source-authored inline interface witness for a
scalar such as `i32`, so a library-defined `Display` interface cannot cover the primitive types
without adding forbidden compiler-known formatting operations.

## What Changes

- Add a Writer-backed formatting model built from `FormatOptions`, `Formatter`, and a static
  `Display` interface whose effectful operation emits through a mutable `Writer` requirement.
- Give formatting options explicit width, alignment, fill, sign, alternate-form, zero-padding,
  precision, and color-permission fields. `Display` remains the default human-readable
  presentation; numeric radix-specific presentations are not hidden inside it.
- Render every integer type without allocation, using bounded local storage and complete Writer
  operations rather than an owned-String intermediary or one write per digit.
- Define coherent scalar conformances: a source interface's defining module may declare that
  interface for a scalar, while nominal providers remain owned exclusively by their defining
  modules.
- Admit and statically select source-authored inline witnesses for scalar providers through the same
  compatibility, reachability, specialization, effect-row, and lowering paths used by nominal
  witnesses.
- **BREAKING** Remove the allocating integer-to-`String` formatting surface and update all standard-
  library callers, tests, examples, generated documentation, and manifests to the Writer-backed
  model. Complete-text integer parsing remains allocation-free and in scope.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-complete-interface-contracts`: Extend coherent conformance ownership and source witness
  admissibility to interface-owned scalar implementations without weakening nominal provider
  locality.
- `bootstrap-type-generics`: Select and specialize source-authored scalar witnesses at ordinary
  named bound-operation call sites, including effectful operations with failure and requirement
  rows.
- `bootstrap-silk-stdlib`: Replace allocating integer rendering with the Writer-backed
  `FormatOptions`, `Formatter`, and `Display` API and its scalar implementations.

## Impact

- Compiler declaration completion, conformance proof, witness discovery, instance reachability, and
  witness-effect lowering for scalar providers.
- Canonical `silk.format`, integer actor modules, and their dependencies on `silk.writer`.
- Public integer formatting APIs and any repository caller currently expecting an owned `String`.
- Standard-library manifests, generated source, language documentation, examples, diagnostics, and
  evaluator/backend acceptance coverage.
- No new intrinsic, compiler-known standard-library actor, runtime dispatch table, allocator
  requirement, or backend-specific formatting implementation.
