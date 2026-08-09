## 1. Shared raw-storage surface

- [x] 1.1 Add `RawBuffer.read<T>(&RawBuffer<T>, usize) -> T` to the intrinsic catalog, presentation, navigation, and instance validation with the non-union Copy boundary.
- [x] 1.2 Add HIR/MIR representation, lowering, deterministic encoding, and verification for shared raw-buffer reads without Slot or storage-state transitions.
- [x] 1.3 Add compiler, ownership, HIR, and MIR tests for valid shared aliases and rejected exclusive, move-only, union, provenance, and bounds cases.

## 2. Engine parity

- [x] 2.1 Execute shared raw-buffer reads in the deterministic evaluator without changing allocation, initializedness, owner, or cleanup state.
- [x] 2.2 Lower verified reads in LLVM using the canonical element layout, checked index, and shared buffer provenance.
- [x] 2.3 Lower verified reads in direct WebAssembly with the same value, trap, zero-sized, and cleanup behavior.
- [x] 2.4 Add evaluator/native/Wasm parity tests covering multiple shared aliases, out-of-bounds traps, subsequent vector move or drop, and absence of read-side allocation.

## 3. Standard library and real programs

- [x] 3.1 Change `Vector.get` to accept `&Vector<T>`, implement it through shared matching and `RawBuffer.read`, and remove the temporary storage-restoration helpers.
- [x] 3.2 Update standard-library, analysis, formatter, completion, hover, navigation, and ownership coverage for the new signature and raw-buffer item.
- [x] 3.3 Rewrite lexer token and diagnostic observation to use shared Vector reads and update its findings.
- [x] 3.4 Rewrite stack-VM step and diagnostic observation to use separate shared Vector reads without depending on structural-union copy, and update its findings.

## 4. Acceptance and publication

- [x] 4.1 Run focused shared-read, Vector, lexer, and stack-VM tests across evaluation, native, and Wasm.
- [x] 4.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` if public package contents or exports changed.
- [x] 4.3 Validate the OpenSpec change strictly, synchronize its delta specs, archive it, and merge the completed branch to main.
