## ADDED Requirements

### Requirement: CRC-32 consumes committed static bytes

The executable CRC-32 example SHALL take its input from a static byte literal and index that
immutable view inside the checksum loop. A substitute fixed array containing the same numeric
values MUST NOT satisfy this example contract.

#### Scenario: Checksum a static literal

- **WHEN** CRC-32 processes the committed bytes `99 13 1d 00`
- **THEN** evaluation, native execution, and direct WebAssembly return the committed checksum with no allocation
