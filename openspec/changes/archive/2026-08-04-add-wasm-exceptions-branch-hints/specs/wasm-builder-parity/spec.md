# wasm-builder-parity Delta

## MODIFIED Requirements

### Requirement: Committed fixtures
The system SHALL commit representative fixture modules covering every supported feature area —
declarations, imports and exports, segments, control flow, and each instruction family,
including SIMD, relaxed SIMD, atomics with shared memories, 64-bit memories, exception handling
with tags and `try_table`, and branch hints — as both expected binary bytes and expected text,
generated deterministically by a repeatable script.

#### Scenario: Fixture verification
- **WHEN** the fixture verification script runs
- **THEN** every fixture's builder output is byte-identical to the committed binary and
  character-identical to the committed text, and every binary passes oracle validation

#### Scenario: Extended-feature fixtures exist
- **WHEN** the fixture inventory is listed
- **THEN** it contains at least one module per new feature family (SIMD, atomics, memory64,
  exceptions, branch hints) verified with the oracle's matching feature flags
