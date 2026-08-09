## Context

See proposal.md — Why. Float values already retain explicit IEEE width/bits and basic operations
avoid fast math. LLVM and WebAssembly do not share a portable sine/cosine instruction, and delegating
to JavaScript `Math`, native `libm`, and a Wasm host import would create three independently rounded
semantics and make standalone Wasm depend on its embedder.

## Goals / Non-Goals

**Goals:**

- Define one reproducible sine/cosine result for each input bit pattern and width.
- Meet a useful finite-input accuracy bound while preserving exact special cases.
- Keep direct Wasm standalone and make FFT genuinely sensitive to the operations.

**Non-Goals:**

- Add the rest of `libm`, complex numbers, SIMD/vector math, or fast-math modes.
- Promise correctly rounded transcendental results.
- Select target-tuned implementations in this bootstrap change.

## Decisions

### D1: Own a canonical software kernel instead of calling host math

A new compiler-owned Transcendental actor defines the versioned constants, range-reduction plan,
polynomial coefficients, special-value mapping, and explicit rounding points for `f32` and `f64`.
The plan is deterministic data consumed by evaluator, LLVM emission, and Wasm emission. Host
`libm`, JavaScript `Math.sin`/`Math.cos`, LLVM transcendental intrinsics, Wasm imports, and fused
multiply-add are not semantic implementations.

This costs more code and runtime work than host calls, but it is the only current option that keeps
exact result-bit parity and standalone Wasm simultaneously.

### D2: Use full-range reduction followed by one shared reduced-domain kernel

The kernel classifies the input from its bits, applies exact special cases, reduces finite
arguments to a quadrant and residual in `[-π/4, π/4]`, then evaluates fixed sine/cosine minimax
polynomials in pinned Horner order. Large arguments use integer-limb Payne–Hanek reduction; a
smaller Cody–Waite path is allowed only when it is proven to produce the same canonical residual.
Coefficients and split constants are stored as exact bit patterns.

Every primitive arithmetic point rounds to the declared width and contraction is forbidden. The
committed conformance table checks quadrant boundaries, tiny values, large magnitudes, signed zero,
infinities, and NaNs against independently generated high-precision reference values.

### D3: Keep explicit transcendental MIR operations

The scalar catalog admits `f32.sin`, `f32.cos`, `f64.sin`, and `f64.cos`; HIR and MIR retain a
width-specific `FloatTranscendental` operation. This keeps diagnostics, encodings, provenance, and
future optimization boundaries explicit. Each engine realizes that operation from the same
canonical plan rather than expanding source-visible helper calls.

### D4: Require exact engine bits and a four-ulp accuracy envelope

Exact engine parity and mathematical accuracy are separate assertions. The kernel's operation order
defines the required result bits; reference-vector tests additionally verify each finite result is
within four ulp of the correctly rounded mathematical value. Changing coefficients or operation
order is therefore a visible semantic change even when both versions satisfy the error bound.

### D5: Replace the FFT's weak oracle with a shifted impulse fingerprint

The FFT uses an impulse away from index zero, causing non-DC bins to contain positive and negative
real/imaginary roots of unity. It scales and converts every selected component into stable integers
before folding them into the entry result. Values are chosen far from conversion boundaries so the
fingerprint is robust within the specified math accuracy but fails if either sine or cosine is
stubbed, sign-flipped, or quadrant-reduced incorrectly.

## Risks / Trade-offs

- [Full-range reduction is larger than the two-operation API suggests] → Keep the public surface to
  sine/cosine and share one reducer; do not weaken semantics to save bootstrap implementation work.
- [LLVM contracts operations or changes rounding] → Emit conservative operations, disable
  contraction, and compare exact native result bits against evaluator vectors.
- [Independent reference vectors are generated incorrectly] → Commit generator provenance and
  cross-check special cases/properties in addition to fixed expected bits.
- [The first kernel is slower than platform math] → Accept predictable bootstrap performance;
  target-tuned kernels can be proposed later only if they retain the same bits or introduce an
  explicit alternative math mode.
