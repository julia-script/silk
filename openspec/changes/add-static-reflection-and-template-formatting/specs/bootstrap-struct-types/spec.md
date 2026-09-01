## ADDED Requirements

### Requirement: Aggregate declarations publish authorized static reflection order

Every concrete nominal aggregate SHALL publish a deterministic reflection kind and declaration
order derived from its existing canonical struct representation. Named tuples and anonymous
positional aggregates SHALL publish ordered positions. Named structs and anonymous named aggregates
SHALL publish ordered labels. Each reflected member SHALL retain its concrete specialized field type
and existing visibility authority without inventing structural compatibility or source-visible
synthetic tuple fields.

#### Scenario: Reflect a named tuple without synthetic labels

- **WHEN** `tuple Point(u32, u32)` is reflected
- **THEN** its descriptor contains positions zero and one with type `u32` and exposes no `_0`, `_1`, or other generated field spelling

#### Scenario: Preserve source order and visibility

- **WHEN** a named struct has public and private fields in declaration order
- **THEN** authorized reflection preserves the relative order of visible public fields while revealing no inaccessible field name

