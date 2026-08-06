## ADDED Requirements

### Requirement: Runtime reachability follows array types and values

Instance discovery SHALL follow canonical array types through reachable contracts, bindings,
construction, indexing, projection, and cleanup and SHALL recursively include the element types
needed for layout and runtime behavior. Unused array declarations or types MUST NOT enter the plan.

#### Scenario: Discover a nested array result

- **WHEN** a reachable factory returns `Array<Array<I32, 4>, 3>`
- **THEN** discovery records the exact outer and inner canonical array types in stable worklist order

### Requirement: Array-bearing instance keys include exact lengths

Instance keys and encodings SHALL distinguish array contracts by canonical element identity and every
nested length without structural abbreviation or backend representation.

#### Scenario: Distinguish equal-layout zero arrays

- **WHEN** two reachable functions use `Array<A, 0>` and `Array<B, 0>`
- **THEN** their type-bearing instance facts remain nominally distinct despite both having zero runtime lanes
