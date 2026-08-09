## ADDED Requirements

### Requirement: MIR represents indexed static-byte reads

MIR SHALL allow a checked slice-element selector whose root is a canonical immutable static byte
view, retain its static-data identity, `usize` length, index local, and source provenance, and produce
one `u8`. Verification SHALL reject selectors whose root is neither a compatible runtime slice nor
a static byte view.

#### Scenario: Verify a static byte selector

- **WHEN** a valid static byte literal is indexed by a runtime `usize`
- **THEN** MIR verification accepts the checked read and its canonical static-data reference

#### Scenario: Reject an incompatible selector root

- **WHEN** malformed MIR applies a static-byte selector to a scalar or aggregate root
- **THEN** verification rejects the module before evaluation or backend emission
