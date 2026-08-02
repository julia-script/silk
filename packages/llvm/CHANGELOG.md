# @silk-effect/llvm

## Unreleased

- Added the Effect-native LLVM builder, typed declarations and function bodies, advanced IR,
  intrinsic catalog, debug metadata, textual IR, and direct bitcode output.
- Replaced the generic `SilkError` API with the LLVM-specific `LlvmError` tagged failure contract.
  Validation now reports discriminated invalid-input and invalid-state reasons, while genuine
  causal failures use a wrapped-failure reason and JavaScript `cause`.
- Made function-body construction a scoped transaction that cleans up reservations and drafts on
  typed failure, validation failure, defects, and interruption without changing the original exit.
- Added data-first and pipeable immutable transformations for `FastMath`, `IntegerMath`, and
  `MemoryAccess`.
- Migrated Effect-returning tests to `it.effect` from `@effect/vitest`, retaining ordinary tests
  for pure behavior.
- Added pinned Zig provenance and differential fixtures, LLVM 22.1.8 interoperability, deterministic
  parity inventory/reporting, benchmark evidence, explicit actor exports, and packed-package
  validation.
