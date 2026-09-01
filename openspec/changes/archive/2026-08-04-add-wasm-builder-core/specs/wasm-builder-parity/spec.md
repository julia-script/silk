# wasm-builder-parity Delta

## Purpose

Anchor the builder's correctness to a pinned external oracle so that emitted binaries and text
are continuously verified against the reference WebAssembly tooling rather than trusted by
construction.

## ADDED Requirements

### Requirement: Pinned oracle

The system SHALL pin one exact `wasm-tools` version as the verification oracle, record the pin
in the package's provenance documentation, and use it for all fixture generation and
verification. Runtime code SHALL never invoke the oracle.

#### Scenario: Oracle version drift

- **WHEN** fixture verification runs against a different oracle version than the recorded pin
- **THEN** verification fails with a message identifying the expected pin

### Requirement: Committed fixtures

The system SHALL commit representative fixture modules covering every baseline feature area —
declarations, imports and exports, segments, control flow, and each instruction family — as
both expected binary bytes and expected text, generated deterministically by a repeatable
script.

#### Scenario: Fixture verification

- **WHEN** the fixture verification script runs
- **THEN** every fixture's builder output is byte-identical to the committed binary and
  character-identical to the committed text, and every binary passes oracle validation

### Requirement: Round-trip agreement

The system SHALL verify for each fixture that the rendered text, parsed by the oracle, produces
a binary byte-identical to the builder's own binary encoding.

#### Scenario: Text round-trip

- **WHEN** a fixture's rendered text is assembled by the pinned oracle
- **THEN** the resulting bytes equal the builder's binary encoding of the same module

### Requirement: Negative validation agreement

The system SHALL maintain a corpus of invalid construction attempts and verify that every case
the builder rejects at define or emit time corresponds to a module the oracle also rejects, so
builder validation never accepts what the specification forbids.

#### Scenario: Invalid corpus agreement

- **WHEN** the negative corpus verification runs
- **THEN** each corpus entry is rejected by the builder with `WasmError`, and any entry
  force-encoded without validation is rejected by the oracle
