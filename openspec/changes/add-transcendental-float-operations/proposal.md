## Why

Silk's floating scalars support basic IEEE arithmetic but cannot express familiar numerical code
that needs trigonometry. The committed radix-2 FFT is blocked on `f64.sin` and `f64.cos`, and its
current unit-impulse/DC oracle would not detect an incorrect implementation after those names exist.

## What Changes

- Add width-preserving `sin` and `cos` operations to both `f32` and `f64`.
- Define special-value behavior, rounding/accuracy guarantees, and cross-engine reproducibility
  explicitly rather than inheriting unspecified host `libm` behavior.
- Carry transcendental operations through semantic facts, HIR, MIR, deterministic evaluation,
  native LLVM, and direct WebAssembly without fast-math assumptions.
- Strengthen the FFT input and result fingerprint so successful execution materially depends on
  trigonometric results, then graduate FFT across all three engines.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-floating-point-scalars`: Define `f32`/`f64` sine and cosine semantics and accuracy.
- `bootstrap-mir`: Represent and verify width-specific transcendental operations.
- `bootstrap-evaluation`: Evaluate sine and cosine deterministically from explicit float bits.
- `bootstrap-backend`: Emit reproducible native and WebAssembly transcendental behavior.
- `bootstrap-algorithm-examples`: Replace FFT's weak oracle and graduate it when math parity lands.

## Impact

- Affects the scalar operation catalog, semantic lowering, MIR, evaluator, LLVM and WebAssembly
  emission/runtime support, float conformance tests, and FFT example.
- Adds no general math module, complex-number type, vectorized math, or broad `libm` surface.
