## MODIFIED Requirements

### Requirement: Backends lower compiler-planned slice shapes

Native LLVM and direct WebAssembly SHALL compute projected borrow addresses from the compiler's
ordered field and fixed-array selectors, including checked runtime indexes and target-planned
element strides. They SHALL preserve the original root as authoritative storage.

#### Scenario: Agree on runtime indexed subplace mutation

- **WHEN** the parity corpus mutates `matrix[index]` through an exclusive inner-array slice
- **THEN** native, Wasm, and evaluation return the same value and trap consistently for an invalid index

### Requirement: Backends realize compiler-planned callable values

Backends SHALL preserve capture construction order while reordering captured lanes by explicit
parameter ordinal at invocation. They MUST NOT infer target argument order from environment field order.

#### Scenario: Agree on staged positional application

- **WHEN** the parity corpus executes `combine(3)(2)(1)`
- **THEN** native, Wasm, and evaluation all invoke `combine(1, 2, 3)`
