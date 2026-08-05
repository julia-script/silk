# wasm-builder-parity Delta

## MODIFIED Requirements

### Requirement: Committed fixtures
The system SHALL commit representative fixture modules covering every supported feature area —
declarations, imports and exports, segments, control flow, and each instruction family,
including SIMD, relaxed SIMD, atomics with shared memories, and 64-bit memories — as both
expected binary bytes and expected text, generated deterministically by a repeatable script.

#### Scenario: Fixture verification
- **WHEN** the fixture verification script runs
- **THEN** every fixture's builder output is byte-identical to the committed binary and
  character-identical to the committed text, and every binary passes oracle validation

#### Scenario: Extended-feature fixtures exist
- **WHEN** the fixture inventory is listed
- **THEN** it contains at least one module per new feature family (SIMD, atomics, memory64)
  verified with the oracle's matching feature flags

## ADDED Requirements

### Requirement: Feature-scoped oracle validation
The system SHALL validate fixtures with an explicit oracle feature list that matches the
package's supported surface exactly, so validation neither assumes unsupported proposals nor
misses supported ones.

#### Scenario: Feature list matches the surface
- **WHEN** the oracle runs after this change
- **THEN** its feature list includes `simd`, `relaxed-simd`, `threads`, and `memory64` in
  addition to the baseline features, and negative-corpus agreement still holds for every rule
