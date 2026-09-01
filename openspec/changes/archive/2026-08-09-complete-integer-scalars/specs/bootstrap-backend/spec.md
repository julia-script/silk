## MODIFIED Requirements

### Requirement: The backend emits checked native arithmetic

LLVM and direct WebAssembly SHALL lower every admitted integer width and mode according to MIR. Ordinary operations trap on pinned overflow/invalid input; named wrapping, saturating, bitwise, shift, rotate, conversion, and checked-Option operations preserve their distinct behavior. Emission SHALL match evaluation and remain deterministic.

#### Scenario: Emit checked signed addition

- **WHEN** ordinary `i32` addition is emitted
- **THEN** generated code detects overflow and reaches the trap path

#### Scenario: Emit wrapping byte addition

- **WHEN** `u8.wrappingAdd` is emitted
- **THEN** both backends wrap at eight bits without the ordinary overflow trap

### Requirement: Backends realize the selected Usize lane exactly

Native LLVM SHALL use the selected unsigned 64-bit `usize` lane; direct WebAssembly SHALL use `i32` with unsigned semantics. Neither backend may narrow native values or choose signed instructions independently.

#### Scenario: Compare Wasm usize values

- **WHEN** values cross the signed `i32` boundary
- **THEN** WebAssembly uses unsigned comparison and matches evaluation
