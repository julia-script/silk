## ADDED Requirements

### Requirement: Backends realize the canonical transcendental contract

Native LLVM and direct WebAssembly SHALL implement MIR sine and cosine with the same canonical
range-reduction constants, operation order, rounding points, and special-value handling as
evaluation. They MUST NOT select target `libm`, ambient host imports, fast-math flags, fused
operations, or target-specific approximations that change result bits.

#### Scenario: Emit standalone Wasm trigonometry

- **WHEN** a Wasm module contains accepted sine and cosine operations
- **THEN** it instantiates without a math host import and returns the canonical bits

#### Scenario: Compare native and evaluator bits

- **WHEN** native code executes the committed transcendental conformance vectors
- **THEN** every result bit pattern matches evaluation exactly
