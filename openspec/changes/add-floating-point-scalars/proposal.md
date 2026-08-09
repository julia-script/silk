## Why

Familiar numerical programs cannot be expressed honestly with integers alone. A focused floating-point change can establish conservative, testable IEEE behavior without mixing it into the already substantial integer migration or prematurely adding a complete math library.

## What Changes

- Add lowercase `f32` and `f64` primitive types.
- Add contextual decimal and exponent literals, defaulting unconstrained float literals to `f64`.
- Add arithmetic, negation, ordinary comparisons, classification, total order, bit reinterpretation, and explicit numeric conversions.
- Preserve signed zero and specified IEEE behavior without implicit fast math.
- Require bit-aware evaluator behavior and LLVM/WebAssembly parity.

## Capabilities

### New Capabilities

- `bootstrap-floating-point-scalars`: Conservative `f32`/`f64` values, literals, operations, conversions, and engine parity.

### Modified Capabilities

- `bootstrap-syntax`: Parse floating fractions and exponent notation losslessly.
- `bootstrap-hir`: Carry canonical float constants, operations, and conversions.
- `bootstrap-callable-values`: Expose `f32` and `f64` primitive actor operations.
- `bootstrap-operator-semantics`: Resolve homogeneous floating arithmetic and comparisons.
- `bootstrap-target-layout`: Plan IEEE binary32 and binary64 layouts and calling lanes.
- `bootstrap-evaluation`: Evaluate floats with explicit width, rounding, and representation behavior.
- `bootstrap-mir`: Represent and verify float constants, operations, and conversions.
- `bootstrap-backend`: Emit conservative LLVM and direct WebAssembly floating operations.
- `language-server-hover`: Render `f32` and `f64` precisely.
- `language-server-completion`: Offer both float types in type positions.

## Impact

The change touches numeric lexing, semantic typing, primitive actors, HIR, MIR, layout, evaluation, LLVM, WebAssembly, deterministic encoders, differential tests, and editor tooling. Transcendental functions, complex numbers, SIMD, and fast-math modes remain separate work.
