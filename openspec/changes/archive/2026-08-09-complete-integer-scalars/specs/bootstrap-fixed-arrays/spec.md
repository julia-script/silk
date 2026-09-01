## MODIFIED Requirements

### Requirement: Indexing is a checked place projection

`subject[index]` SHALL require a `usize` index. A known out-of-range literal SHALL fail analysis; a dynamic index SHALL check `index < length` and trap before projection. Place composition and ownership behavior remain unchanged.

#### Scenario: Read a dynamic element

- **WHEN** `Array<i32, 4>` is indexed by runtime `usize`
- **THEN** execution checks the canonical length and returns the selected element or traps
