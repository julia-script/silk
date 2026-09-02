## ADDED Requirements

### Requirement: Native backends emit one C thunk per export

Supported native LLVM emission SHALL define, for each discovered export, one external function
under the target's C calling convention and the export symbol with the LLVM types its classified C
signature selects, whose body forwards the arguments to the export's ordinary implementation
function and returns its result. The implementation SHALL keep its compiler-versioned symbol and
internal signature. The artifact SHALL record exports with their symbols and signatures in
deterministic order. Planning for a WebAssembly target SHALL reject an export before construction under either backend.

#### Scenario: Emit a thunk

- **WHEN** `export "C" fn silk_test_double_v1(value: i32) -> i32` is emitted
- **THEN** the LLVM module contains `define i32 @silk_test_double_v1(i32)` with calling convention property `0` whose body is one call to the implementation symbol and one `ret`, and the implementation symbol is distinct

#### Scenario: Keep bitcode deterministic

- **WHEN** identical target-aware MIR with exports is emitted in fresh processes
- **THEN** the bitcode is byte-identical
