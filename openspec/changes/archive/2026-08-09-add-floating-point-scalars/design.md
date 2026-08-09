## Context

This change follows `complete-integer-scalars` and extends its scalar catalog, exact conversion boundary, MIR vocabulary, and parity matrix. JavaScript numbers alone are insufficient for explicit `f32` rounding, signed-zero representation, and same-width bit reinterpretation.

## Goals / Non-Goals

**Goals:** conservative IEEE binary32/binary64 semantics and exact evaluator/LLVM/Wasm agreement.

**Non-Goals:** transcendental math library, complex/SIMD values, decimal floats, arbitrary precision, or fast math.

## Decisions

### Float values are width-plus-bits in the evaluator

Store canonical IEEE bits, round after each `f32` operation, preserve signed zero, and canonicalize arithmetic NaNs when payload is unspecified. Bit reinterpretation preserves bits exactly.

*Alternative considered:* bare JavaScript numbers. Rejected because width and representation operations become implicit.

### Literal parsing controls rounding

Parse decimal source deterministically into correctly rounded binary32/binary64 rather than relying on uncontrolled intermediate coercions. Encoders publish canonical bits.

### Backends receive no implicit fast-math promises

LLVM uses ordinary operations without flags; Wasm uses corresponding float instructions. Tests compare exact finite results/round trips and specified NaN classification/order behavior.

## Risks / Trade-offs

- [NaN payloads vary] → require payload bits only for representation round trips, not unspecified arithmetic NaNs.
- [Decimal conversion is subtle] → use a dedicated verified conversion path and boundary fixtures.
- [FFT still needs math APIs] → record missing operations in the later algorithm change instead of expanding this change.

## Migration Plan

Extend the catalog and syntax, then HIR/MIR/evaluator, then both backends and editor tooling; gate each with float boundary fixtures.

## Open Questions

None.

