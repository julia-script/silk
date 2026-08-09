## ADDED Requirements

### Requirement: Static byte views support checked indexing

An immutable static byte view SHALL expose its `usize` byte length and support ordinary runtime
indexing that produces one `u8` without allocation. Indexing MUST use the same bounds behavior and
source provenance as an immutable runtime slice.

#### Scenario: Read committed binary bytes

- **WHEN** source indexes `b"\x99\x13\x1d\x00"` at each valid position
- **THEN** it observes `153`, `19`, `29`, and `0` in order without allocating

#### Scenario: Reject a static-byte overrun

- **WHEN** source indexes a four-byte static view at index four
- **THEN** execution traps at the indexing expression with the same bounds contract as a shared slice
