## MODIFIED Requirements

### Requirement: Slice length and indexing are safe runtime operations

Slice `length` and indices SHALL use target-selected `usize`. Access SHALL check `index < length` and trap before projection or replacement evaluation. Zero-length and zero-sized-element slices SHALL retain their logical lengths.

#### Scenario: Traverse a slice

- **WHEN** a `usize` cursor indexes a runtime-length slice
- **THEN** each in-range access selects the corresponding element without signed bounds logic

#### Scenario: Trap before replacement

- **WHEN** an exclusive index equals or exceeds length
- **THEN** execution traps before evaluating the replacement
