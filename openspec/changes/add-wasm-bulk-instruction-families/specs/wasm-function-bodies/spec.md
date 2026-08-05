# wasm-function-bodies Delta

## ADDED Requirements

### Requirement: SIMD instruction coverage
The system SHALL provide constructors, encoding, text rendering, and validation for the
fixed-width SIMD instruction set — `v128.const` with a 16-byte immediate, loads and stores with
splat, zero-extend, and lane variants, lane access with immediate lane indices,
`i8x16.shuffle` with a 16-lane immediate, and the arithmetic, comparison, bitwise, and
conversion families — and for the standardized relaxed SIMD instructions.

#### Scenario: Lane index validated
- **WHEN** a body uses `f32x4.extract_lane` with lane index 4
- **THEN** definition fails with `WasmError` because the lane index must be below the lane count

#### Scenario: Shuffle immediate validated
- **WHEN** a body uses `i8x16.shuffle` with a lane selector outside 0..31
- **THEN** definition fails with `WasmError`

#### Scenario: SIMD round-trips
- **WHEN** a committed body uses `v128.const`, a shuffle, and lane accesses
- **THEN** both emitted representations encode the immediates exactly and agree with each other

### Requirement: Atomic instruction coverage
The system SHALL provide constructors, encoding, text rendering, and validation for the atomic
instruction family — atomic loads and stores, read-modify-write operations, compare-exchange,
`memory.atomic.wait32`/`wait64`/`notify`, and `atomic.fence` — requiring exact natural
alignment for every atomic memory access.

#### Scenario: Atomic alignment is exact
- **WHEN** a body uses `i32.atomic.load` with an alignment below the access width
- **THEN** definition fails with `WasmError`

#### Scenario: Atomic operations on unshared memory
- **WHEN** a committed body applies atomic operations to an unshared memory
- **THEN** definition succeeds, as the specification permits atomics on unshared memories

### Requirement: Address-type-aware body validation
The system SHALL type every address operand against the address type of the memory or table the
instruction references: loads, stores, and bulk memory operations take addresses of the
memory's address type, and `memory.size`/`memory.grow`/table counterparts produce and consume
that address type.

#### Scenario: 64-bit memory addressing
- **WHEN** a body loads from a 64-bit memory using an `i64` address and stores the result
- **THEN** definition succeeds, and using an `i32` address for the same access fails with
  `WasmError`
