# @silk-effect/wasm

## 0.0.1

### Patch Changes

- d8ad798: Introduce `@silk-effect/wasm`: Effect-native WebAssembly module construction with instructions as
  plain data, handle-based index spaces resolved at emission, full specification validation at
  define and emit time, and deterministic `.wat` text and `.wasm` binary output verified against a
  pinned `wasm-tools` oracle. Baseline: core 2.0 plus tail calls, extended constant expressions,
  and multiple memories.
- 689d5ad: Add the bulk instruction families to `@silk-effect/wasm`: fixed-width SIMD and relaxed SIMD
  (`v128.const`, shuffles, lane operations, and the full `0xFD` family), threads (shared memories
  and the `0xFE` atomic family with exact-alignment validation), and memory64 (64-bit addressed
  memories and tables threaded through validation, limits, and both emitters). Baseline modules
  emit byte-identical output; the pinned oracle validates and round-trips an exhaustive
  per-opcode fixture.
- c3c5b2a: Add exception handling and branch hinting to `@silk-effect/wasm`: exception tags as a new
  importable/exportable entity kind, the `exnref` reference type, `throw`/`throw_ref`/`try_table`
  with all four catch-clause kinds, tag names in the name section, and optional likely/unlikely
  hints on `br_if`/`if` emitted as the `metadata.code.branch_hint` custom section and text
  annotations. Legacy exception handling remains permanently excluded.
- 0cc388f: Add GC and typed function references to `@silk-effect/wasm`, completing the Chrome-unflagged
  feature surface: parameterized reference types with abstract and concrete heap types, struct
  and array types in canonicalized recursive groups with declared supertypes, subtype-aware
  validation, the GC/cast/typed-call instruction set, and type names. `ValType.RefType` changes
  shape to `{ nullable, heapType }` with the classic shorthands preserved; baseline modules emit
  byte-identical output.
