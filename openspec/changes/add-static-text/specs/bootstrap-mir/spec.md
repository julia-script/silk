## ADDED Requirements

### Requirement: MIR represents deterministic static data

MIR SHALL carry a canonical ordered static-data table plus immutable views with `usize` lengths. Verification SHALL reject mismatched contents, lengths, mutability, or missing entries; encoding SHALL remain deterministic.

#### Scenario: Verify a literal view

- **WHEN** a view references a static-data entry with matching length
- **THEN** MIR verification accepts it without an allocation operation

