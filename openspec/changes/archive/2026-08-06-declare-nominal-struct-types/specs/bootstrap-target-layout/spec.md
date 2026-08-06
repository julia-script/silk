## ADDED Requirements

### Requirement: Nominal layout facts precede runtime reachability

After declaration type dependencies resolve and a target is selected, the compiler SHALL compute
one immutable nominal layout catalog for every concrete non-generic struct in the loaded closure,
including unused private declarations. The catalog SHALL retain available and unavailable entries
under canonical nominal identities so analysis and tooling can inspect physical representation
before instance discovery or backend work. Runtime layout planning SHALL select reachable entries
from this catalog rather than recomputing their fields.

#### Scenario: Inspect an unused struct layout

- **WHEN** a module declares a valid private struct that no runtime instance reaches
- **THEN** the nominal catalog contains its target-aware layout while the runtime layout plan omits it

#### Scenario: Reuse a catalog entry in the runtime plan

- **WHEN** instance discovery reaches a struct already present in the nominal catalog
- **THEN** the runtime plan uses the catalog's identical size, alignment, and offsets without a second layout decision

#### Scenario: Catalog an unavailable declaration

- **WHEN** a struct contains an unknown type or inline recursive dependency
- **THEN** the catalog retains that struct's unavailable layout state and cause while other entries remain available

### Requirement: Nominal struct layouts are target-aware compiler facts

For each concrete nominal struct, its catalog entry SHALL recursively include every field type
needed for its representation and SHALL record the struct's size, alignment, and each field's
physical offset in declaration order. Each field offset SHALL be the smallest offset satisfying its
field alignment after the preceding field; the completed size SHALL include tail padding to the
struct alignment. An empty struct SHALL have size zero and alignment one. These facts SHALL be
selected by the compiler before MIR lowering and MUST NOT be recomputed or changed by a backend.

#### Scenario: Lay out scalar fields with padding

- **WHEN** a reachable struct declares fields whose selected-target alignments require padding
- **THEN** the layout records declaration-ordered offsets, internal padding, maximum field alignment, and tail-padded size

#### Scenario: Lay out a nested struct

- **WHEN** a reachable struct contains another available nominal struct
- **THEN** the plan contains both canonical entries and computes the outer offset and size from the inner compiler-owned layout

#### Scenario: Lay out an empty struct

- **WHEN** an empty marker struct is reachable
- **THEN** its canonical layout entry records size zero, alignment one, and no fields

### Requirement: Struct layout planning is finite and deterministic

Struct layout SHALL follow canonical nominal type dependencies rather than source traversal order.
An unavailable field type or inline-recursive dependency SHALL make only dependent struct layouts
unavailable with their originating causes; unrelated scalar and struct layouts SHALL remain
available. Identical target and declaration inputs SHALL produce byte-identical ordered entries and
field offsets across fresh processes.

#### Scenario: Refuse an inline recursive layout

- **WHEN** a reachable nominal struct participates in a direct or mutual inline dependency cycle
- **THEN** its layout remains unavailable and no placeholder size or backend type is created

#### Scenario: Propagate an unavailable nested layout

- **WHEN** an outer struct contains a struct whose field type is unavailable
- **THEN** the outer layout is unavailable with that dependency cause while unrelated entries remain complete

#### Scenario: Repeat aggregate layout planning

- **WHEN** the same nested nominal types are planned repeatedly for one target
- **THEN** their canonical entry order, sizes, alignments, field offsets, and encoding are byte-identical
