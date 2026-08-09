# bootstrap-static-text Specification

## Purpose
Provide immutable static UTF-8 and byte literals for program data and output without fixing the representation or service model of Silk's eventual owning string type.
## Requirements
### Requirement: Static text literals preserve Unicode content

A quoted text literal SHALL decode escapes into immutable program-lifetime UTF-8, retain exact source provenance, and expose its bytes and `usize` byte length without allocation. It MUST NOT define the owning `string` layout, mutability, or target representation.

#### Scenario: Materialize non-ASCII text

- **WHEN** a literal contains Unicode scalar values and escapes
- **THEN** its view exposes the exact decoded UTF-8 bytes on every target

### Requirement: Static byte literals preserve exact bytes

A byte-string literal SHALL expose immutable program-lifetime `u8` values. Decoded values outside the byte range and malformed escapes SHALL produce deterministic diagnostics without partial data.

#### Scenario: Materialize bytes

- **WHEN** source contains `b"life\n"`
- **THEN** the view exposes four ASCII letters and one newline byte

### Requirement: Static storage is target-neutral compiler data

Literal identity and content SHALL be deterministic before backend selection. Backends MAY place or coalesce data differently, but observable bytes, length, immutability, and lifetime MUST match evaluation.

#### Scenario: Reuse a literal

- **WHEN** one literal is referenced multiple times
- **THEN** all references observe identical bytes regardless of storage coalescing

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
