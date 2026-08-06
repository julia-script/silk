## ADDED Requirements

### Requirement: Arrays are canonical logical HIR values

HIR contracts, parameters, bindings, calls, and results SHALL carry canonical array types containing
their logical element type and length. A complete literal SHALL lower to one typed construction with
element expressions in ascending index order and exact source evaluation provenance.

#### Scenario: Elaborate a complete literal

- **WHEN** semantic facts accept `[first(), second()]` as `Array<I32, 2>`
- **THEN** HIR retains left-to-right initializer evaluation and one canonical two-element construction

### Requirement: Checked indexing is typed HIR place projection

HIR SHALL represent indexing with its subject expression, canonical array type, index expression,
element result type, access mode, bounds mode, and exact span. Mixed index and field projection chains
SHALL remain nested in source order. A requested non-Copy element move SHALL remain explicit for
ownership to reject.

#### Scenario: Elaborate a dynamic indexed field read

- **WHEN** source reads `pairs[index].left`
- **THEN** HIR contains a checked index place followed by the canonical `Pair.left` Copy read
