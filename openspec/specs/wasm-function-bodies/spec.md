# wasm-function-bodies Specification

## Purpose

Represent function bodies as plain immutable instruction data for the baseline feature set and
validate each body against the WebAssembly specification's typing rules when it is committed.

## Requirements

### Requirement: Instructions as immutable data
The system SHALL represent every instruction as a plain immutable value in a discriminated
union, constructible without a builder, and SHALL represent a function body as an ordinary
read-only array of instructions plus declared locals. Entity references inside instructions
SHALL be handles, not numeric indices.

#### Scenario: Bodies compose as values
- **WHEN** a caller builds two instruction arrays and concatenates them into one body
- **THEN** the combined body commits successfully with behavior identical to writing the
  sequence directly

### Requirement: Structured control flow as nested data
The system SHALL express `block`, `loop`, and `if`/`else` as instruction variants containing
nested instruction sequences and a block type, with branches referring to enclosing structures
by relative depth as in the specification.

#### Scenario: Nested branch depth
- **WHEN** a body branches with depth 1 from inside a `loop` nested in a `block`
- **THEN** the committed body targets the outer `block` in both emitted representations

### Requirement: Baseline instruction coverage
The system SHALL provide constructors, encoding, text rendering, and validation for every
instruction in WebAssembly core 2.0 — including multi-value blocks and calls, bulk memory and
table operations, reference-type operations, sign extension, saturating float-to-int
truncation, and mutable-global access — plus tail calls (`return_call`, `return_call_indirect`)
and multi-memory immediates.

#### Scenario: Bulk memory operation round-trips
- **WHEN** a committed body uses `memory.copy` between two declared memories
- **THEN** both emitted representations encode the instruction with the correct memory indices

### Requirement: Define-time full validation
The system SHALL validate each function body at definition time using the specification's
validation algorithm — value-stack typing, control-frame tracking, branch-target arity, local
and entity reference checking, and polymorphic typing after unreachable code — and SHALL reject
invalid bodies with `WasmError` before any state is committed.

#### Scenario: Stack underflow rejected
- **WHEN** a body applies `i32.add` with one value on the stack
- **THEN** definition fails with `WasmError` and the function remains undefined

#### Scenario: Polymorphic unreachable accepted
- **WHEN** a body follows `unreachable` with instructions that only type-check against a
  polymorphic stack and ends with correct result arity
- **THEN** definition succeeds as required by the specification's validation algorithm

#### Scenario: Failed definition is retryable
- **WHEN** a definition fails validation and the caller retries with a corrected body
- **THEN** the corrected definition succeeds and no artifact of the failed attempt is observable

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

### Requirement: Exception instruction coverage
The system SHALL provide constructors, encoding, text rendering, and validation for `throw`
(popping the tag's parameter types), `throw_ref` (popping an `exnref`), and `try_table` — a
structured block whose catch clauses each target an enclosing label by relative depth. A
`catch` clause requires its label to accept the tag's parameters, `catch_ref` the parameters
plus `exnref`, `catch_all` nothing, and `catch_all_ref` exactly `exnref`. `exnref` SHALL be
usable wherever a reference type is.

#### Scenario: try_table catches into a matching label
- **WHEN** a body wraps a `throw` in a `try_table` whose `catch` clause targets a block
  accepting the tag's parameters
- **THEN** definition succeeds and both emitted representations agree

#### Scenario: Catch label arity mismatch rejected
- **WHEN** a `catch` clause targets a label that does not accept the tag's parameter types
- **THEN** definition fails with `WasmError`

#### Scenario: throw_ref requires an exnref
- **WHEN** a body applies `throw_ref` to an `i32`
- **THEN** definition fails with `WasmError`

### Requirement: Branch hints
The system SHALL accept an optional `likely` or `unlikely` hint on `br_if` and `if`
instructions, preserve it through both emitted representations, and reject hints anywhere the
specification does not define them.

#### Scenario: Hinted branch round-trips
- **WHEN** a body marks a `br_if` as likely and the module is emitted
- **THEN** the binary carries a branch-hint entry for that instruction's byte offset and the
  text carries the corresponding annotation
