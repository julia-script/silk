## 1. Intrinsic Boundary

- [x] 1.1 Add the unsafe `Intrinsic.rawBufferCopy` and `Intrinsic.rawBufferFill` catalog entries with
      their parameter shapes, unit results, and all-three-target availability.
- [x] 1.2 Add their HIR operation identities and MIR operations, including the element stride and the
      structural-Copy classification the engines share.
- [x] 1.3 Verify the operand types, the exclusive destination, the shared source range, and the
      recorded classification during MIR verification.

## 2. Engines

- [x] 2.1 Evaluate the copy as if through an intermediate buffer, give up the moved-from slots of a
      move-only element, and trap on a released or uninitialized source range.
- [x] 2.2 Evaluate the fill over the selected byte range and trace both operations.
- [x] 2.3 Lower the copy to `llvm.memmove` and the fill to `llvm.memset` with bounds traps.
- [x] 2.4 Lower the copy to `memory.copy` and the fill to `memory.fill` with the same bounds traps.

## 3. Standard Library

- [x] 3.1 Expose `RawBuffer.copy` and `RawBuffer.fill` and regenerate the shipped source table.
- [x] 3.2 Migrate `Vector` growth to one bulk move per migration.
- [x] 3.3 Add `u8`-concrete `Vector.appendBytes` and route `Bytes.append` through it.
- [x] 3.4 Regenerate the standard-library documentation page.

## 4. Acceptance

- [x] 4.1 Three-engine parity for a copied range, a filled byte range, vector growth, and
      `Bytes.append`.
- [x] 4.2 A move-only copy whose moved-from slots are empty afterwards.
- [x] 4.3 An overlapping copy, aliased on lowered MIR, agreeing on all three engines.
- [x] 4.4 An out-of-range copy trapping on the evaluator and Wasm.
