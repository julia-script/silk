## 1. Frontend and Semantics

- [x] 1.1 Add fraction/exponent syntax with recovery and exact source preservation.
- [x] 1.2 Extend the scalar catalog with `f32`/`f64` and implement contextual/default correctly rounded literals.
- [x] 1.3 Add primitive arithmetic, comparison, classification, total-order, bit, and conversion contracts plus operators/callables.

## 2. Middle End and Evaluation

- [x] 2.1 Add float HIR/MIR constants, operations, conversions, verification, provenance, and deterministic encodings.
- [x] 2.2 Add canonical layouts/calling lanes.
- [x] 2.3 Implement width-plus-bits evaluation, `f32` rounding, signed zero, NaN policy, total order, and reinterpretation.

## 3. Backends and Tooling

- [x] 3.1 Add conservative LLVM float lowering without implicit fast-math flags.
- [x] 3.2 Add direct Wasm float lowering and exact constant/conversion handling.
- [x] 3.3 Add hover/completion and differential boundary fixtures.

## 4. Verification

- [x] 4.1 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`.
