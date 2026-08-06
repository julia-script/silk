## ADDED Requirements

### Requirement: Array literal facts retain every element and one complete type outcome

Semantic analysis SHALL retain each array literal element in source order with its expression type,
contextual expectation, and compatibility outcome. It SHALL separately publish the canonical element
type, expected or inferred length, and complete construction outcome without dropping independent
element failures.

#### Scenario: Retain incompatible elements

- **WHEN** a literal contains compatible `I32` elements and one `Bool` element
- **THEN** every element fact remains queryable and the literal has an explicit incompatible-type outcome

### Requirement: Index facts expose place and bounds knowledge

Each index step SHALL retain the subject fact, canonical array type, index expression and type,
canonical element type, access request, exact provenance, and whether bounds are statically valid,
statically invalid, or require a runtime check. Invalid subject or index facts SHALL make dependent
steps causally unavailable without alternate lookup.

#### Scenario: Diagnose a constant out-of-bounds index

- **WHEN** `values[4]` indexes `Array<I32, 4>`
- **THEN** the index fact records the canonical length and literal index and reports a stable semantic diagnostic

#### Scenario: Retain a dynamic bounds check

- **WHEN** an available `I32` parameter indexes an array
- **THEN** the fact records a required runtime check and the canonical element type
