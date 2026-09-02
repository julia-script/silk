## MODIFIED Requirements

### Requirement: Struct layout planning is finite and deterministic

Struct layout SHALL follow canonical nominal type dependencies rather than source traversal order.
An unavailable field type or inline-recursive dependency SHALL make only dependent struct layouts
unavailable with their originating causes; unrelated scalar and struct layouts SHALL remain
available. The layout of a compiler-owned indirection SHALL be independent of the layout of the type
it indirects, so an indirected element type SHALL NOT be a layout dependency of the struct holding
the indirection, and a struct that reaches itself only through an indirection SHALL have a finite,
available layout. Identical target and declaration inputs SHALL produce byte-identical ordered
entries and field offsets across fresh processes.

#### Scenario: Refuse an inline recursive layout

- **WHEN** a reachable nominal struct participates in a direct or mutual inline dependency cycle
- **THEN** its layout remains unavailable and no placeholder size or backend type is created

#### Scenario: Propagate an unavailable nested layout

- **WHEN** an outer struct contains a struct whose field type is unavailable
- **THEN** the outer layout is unavailable with that dependency cause while unrelated entries remain complete

#### Scenario: Repeat aggregate layout planning

- **WHEN** the same nested nominal types are planned repeatedly for one target
- **THEN** their canonical entry order, sizes, alignments, field offsets, and encoding are byte-identical

#### Scenario: Lay out a struct that reaches itself through indirection

- **WHEN** a reachable struct holds an explicit heap indirection to its own type
- **THEN** the catalog entry records a complete size, alignment, and field offsets, and planning terminates without visiting the struct a second time

#### Scenario: Exclude an indirected element from layout dependencies

- **WHEN** a struct holds a compiler-owned indirection over an element type
- **THEN** the struct's catalog entry is computed from the indirection's own fixed representation and does not require the element's layout
