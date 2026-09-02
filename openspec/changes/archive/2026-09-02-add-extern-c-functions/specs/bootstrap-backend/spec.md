## ADDED Requirements

### Requirement: Native backends declare reachable foreign functions under the C calling convention

Supported native LLVM emission SHALL declare each reachable foreign symbol exactly once as an
external function under the target's C calling convention (LLVM convention `0`, which the textual
renderer prints without a marker) with the LLVM types the classified C signature selects:
exact-width integers, the target pointer-width integer for `isize`/`usize`, `float`/`double`, and
`void` for a `()` result. Each foreign-call operation SHALL emit one direct call to that
declaration and SHALL reload every address-taken root afterwards, as synchronous Silk calls do. The
artifact SHALL record the reachable foreign imports with their signatures in deterministic order;
an unreachable foreign declaration SHALL leave no trace in the module or the inventory.

#### Scenario: Declare once and call directly

- **WHEN** two reachable functions each call `silk_test_add`
- **THEN** the LLVM module contains exactly one `declare i32 @silk_test_add(i32, i32)` whose calling convention property is `0`, and two direct `call` instructions to it

#### Scenario: Select the target pointer width

- **WHEN** a foreign function takes `usize` and the target is a 64-bit native target
- **THEN** the declaration's parameter type is `i64`

### Requirement: Non-native surfaces receive no foreign function ABI

Direct-WebAssembly emission, LLVM emission for a WebAssembly target, and the evaluator MUST NOT
lower a foreign-call operation to an invented import, host shim, or adapter. Availability
validation SHALL reject a reachable foreign call for those surfaces before backend or evaluator
construction, and SHALL allow programs whose closure contains none.

#### Scenario: Reject a reachable foreign call under direct Wasm

- **WHEN** a direct-Wasm entry reaches a foreign function
- **THEN** planning reports foreign-function target unavailability and no partial module is constructed
