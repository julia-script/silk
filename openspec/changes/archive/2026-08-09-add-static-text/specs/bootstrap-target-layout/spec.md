## ADDED Requirements

### Requirement: Static data placement is compiler-owned target data

Layout planning SHALL retain exact bytes, required alignment, immutable address lane, and target-selected `usize` length lane without publishing an owning String ABI.

#### Scenario: Plan Wasm static text

- **WHEN** a Wasm program reaches a text literal
- **THEN** layout plans one immutable byte region and a 32-bit `usize` length lane
