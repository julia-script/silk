## 1. Canonical transcendental kernel

- [x] 1.1 Add a Transcendental actor that owns exact-bit constants, range-reduction data, polynomial coefficients, operation order, and special-value mappings for both widths
- [x] 1.2 Implement canonical signed-zero, NaN, infinity, tiny-input, quadrant, and full-range finite classification from explicit float bits
- [x] 1.3 Implement shared range reduction and fixed-order sine/cosine reduced-domain kernels with explicit width rounding and no fused operations
- [x] 1.4 Commit independently generated high-precision conformance vectors with generator provenance and verify the four-ulp accuracy envelope

## 2. Source and semantic surface

- [x] 2.1 Add width-preserving `sin` and `cos` operations to the `f32` and `f64` scalar catalogs
- [x] 2.2 Elaborate valid calls into explicit width-specific HIR operations and reject wrong arity, cross-width arguments, and unsupported spellings
- [x] 2.3 Extend semantic, HIR encoding, navigation, and determinism tests for all four admitted operations

## 3. MIR contract

- [x] 3.1 Add explicit `FloatTranscendental` MIR operations for `Sin` and `Cos` with width and provenance
- [x] 3.2 Lower the four scalar operations into MIR and deterministically encode their operation names and widths
- [x] 3.3 Verify same-width floating operands/results and reject integer, mismatched-width, and unknown-operation MIR

## 4. Deterministic evaluation

- [x] 4.1 Evaluate MIR sine and cosine through the canonical kernel without ambient JavaScript math semantics
- [x] 4.2 Round every declared operation point to `f32` or `f64` and publish exact result bits
- [x] 4.3 Test all committed vectors, repeated-evaluation determinism, special values, symmetry, and quadrant properties

## 5. Native and WebAssembly realization

- [x] 5.1 Emit the canonical kernel operation order in LLVM without host `libm`, transcendental intrinsics, fast math, or contraction
- [x] 5.2 Emit the canonical kernel operation order in direct WebAssembly without math imports
- [x] 5.3 Compare exact evaluator/native/WebAssembly result bits for the full conformance table on both widths
- [x] 5.4 Verify emitted symbol sets, text, binaries, and results remain deterministic in fresh processes

## 6. FFT graduation

- [x] 6.1 Replace the index-zero unit impulse with a shifted impulse whose non-DC components exercise sine and cosine
- [x] 6.2 Fold robustly scaled real and imaginary components into an exact entry fingerprint that fails for stubbed or quadrant-broken math
- [x] 6.3 Execute FFT through evaluation, native, and direct WebAssembly, then change its manifest to executable status and remove the math blockers
- [x] 6.4 Update the FFT README and capability inventory with the strengthened input, oracle, and transcendental contract

## 7. Verification

- [x] 7.1 Run focused scalar, semantic, HIR, MIR, evaluator, backend, determinism, and algorithm tests
- [x] 7.2 Run `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, and `pnpm check`
