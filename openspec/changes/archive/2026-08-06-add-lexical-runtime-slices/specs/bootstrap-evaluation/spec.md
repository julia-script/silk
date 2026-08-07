## ADDED Requirements

### Requirement: Evaluation preserves borrowed backing identity

Logical evaluation SHALL realize a slice as a view of one stable caller-owned storage place with a
base position and runtime length, not as a copied array value. Shared reads and exclusive writes
SHALL therefore observe the same backing state across nested ordinary calls while access mode and
loan identity remain compiler facts rather than runtime payload.

#### Scenario: Observe exclusive mutation in the caller

- **WHEN** a caller passes `&mut values` to a helper that replaces an indexed element and then reads `values` after return
- **THEN** logical evaluation observes the helper's replacement in the original caller-owned array

#### Scenario: Read two source lengths through one callee

- **WHEN** one evaluated slice function receives arrays of two different lengths
- **THEN** each invocation traverses exactly its runtime logical length without copying or specializing the callee by length

### Requirement: Slice evaluation preserves checked-place ordering

Evaluation SHALL check a slice index against its own runtime length before reading, projecting, or
evaluating an assignment replacement. Valid exclusive replacement SHALL update the authoritative
backing place and clean the displaced value exactly once.

#### Scenario: Trap without evaluating a replacement

- **WHEN** an exclusive slice write is out of bounds and its replacement would otherwise produce an observable trace event
- **THEN** evaluation traps at the bounds check and records no replacement event or write
