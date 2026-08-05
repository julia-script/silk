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
