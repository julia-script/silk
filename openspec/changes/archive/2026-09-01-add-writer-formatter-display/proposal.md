## Why

Silk's integer formatting currently allocates an owned `String`, which makes presentation depend on
the allocator and prevents generic values from streaming directly through the existing `Writer`
service. The language also cannot currently select a source-authored inline interface witness for a
scalar such as `i32`, so a library-defined `Display` interface cannot cover the primitive types
without adding forbidden compiler-known formatting operations. Once selected, a borrowed scalar
witness still has no ordinary source expression for reading its `Copy` referent, and value-reference
parameters cannot currently perform the compatible reborrows already promised by the reference
model.

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
- Add postfix `reference.*` as an ordinary referent-place projection. A bare projection reads only a
  `Copy` referent, while `&reference.*` and `&mut reference.*` form compatible reborrows without an
  intrinsic or implicit reference-to-value conversion.
- Keep reference `Copy` semantics sealed: shared references remain compiler-proven `Copy`, exclusive
  references remain affine, and redundant or alias-unsafe source `Copy` implementations for
  references are rejected.
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
- `bootstrap-syntax`: Parse `.*` as a repeated postfix place projection that composes with field,
  index, call, borrow, assignment, and precedence syntax.
- `bootstrap-syntax-correspondence`: Preserve referent-projection identity and tokens through
  lossless syntax formatting, source correspondence, and incremental edits.
- `bootstrap-semantic-facts`: Publish the referent target, access, provenance, availability, and
  projection-chain identity for each `.*` occurrence.
- `bootstrap-ownership`: Read a referent only when it is `Copy`, preserve its owner, reject moves
  through borrowed storage, and enforce shared versus exclusive reborrow access.
- `bootstrap-runtime-slices`: Bring value-reference call-scoped reborrowing into alignment with the
  existing slice reborrow model.
- `bootstrap-hir`: Represent typed referent-place projections and their borrow provenance without
  erasing them into an intrinsic call.
- `bootstrap-mir`: Lower and verify referent reads and reborrows as ordinary typed place operations.
- `bootstrap-evaluation`: Execute referent reads and reborrows with the same value and ownership
  behavior as other canonical places.
- `bootstrap-backend`: Lower verified referent projections consistently for native and Wasm targets.
- `bootstrap-silk-stdlib`: Replace allocating integer rendering with the Writer-backed
  `FormatOptions`, `Formatter`, and `Display` API and its scalar implementations.

## Impact

- Compiler declaration completion, conformance proof, witness discovery, instance reachability, and
  witness-effect lowering for scalar providers.
- Parser, formatter, semantic facts, ownership, HIR, MIR, evaluator, and native/Wasm lowering for
  postfix referent projection and compatible value-reference reborrowing.
- Canonical `silk.format`, integer actor modules, and their dependencies on `silk.writer`.
- Public integer formatting APIs and any repository caller currently expecting an owned `String`.
- Standard-library manifests, generated source, language documentation, examples, diagnostics, and
  evaluator/backend acceptance coverage.
- No new intrinsic, compiler-known standard-library actor, runtime dispatch table, allocator
  requirement, implicit reference-to-value conversion, or backend-specific formatting
  implementation.
