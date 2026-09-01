# wasm-output Delta

## ADDED Requirements

### Requirement: Extended-feature encodings

The system SHALL encode and render the SIMD (`0xFD` prefix), atomic (`0xFE` prefix), shared
and 64-bit limits, and 64-bit memarg forms exactly as the binary and text formats specify, and
these forms SHALL satisfy the same determinism, oracle-validation, and text-to-binary
round-trip guarantees as the baseline output.

#### Scenario: Shared memory limits flags

- **WHEN** a module with a shared memory is encoded
- **THEN** the memory's limits carry the shared flag and the binary passes oracle validation

#### Scenario: Extended forms round-trip

- **WHEN** a module using SIMD constants, atomic operations, and a 64-bit memory is rendered as
  text and assembled by the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding
