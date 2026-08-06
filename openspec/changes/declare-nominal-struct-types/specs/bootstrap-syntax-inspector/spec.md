## ADDED Requirements

### Requirement: Inspect nominal struct declarations and layouts

The docs site SHALL extend its facade-only declaration and layout inspection surfaces to present
each nominal struct's canonical identity, visibility, ordered fields, resolved type dependencies,
field visibility, exact provenance, target-selected size and alignment, physical offsets, and
padding. Missing, duplicate, inaccessible, leaking-private, recursive, and transitively unavailable
states SHALL remain explicit beside their phase-owned diagnostics. The labs SHALL keep state in
browser memory and provide accessible text equivalents for every graphical layout relationship.

#### Scenario: Inspect a padded struct

- **WHEN** a developer selects a struct whose fields require padding on the selected target
- **THEN** the lab shows declaration order beside exact offsets, padding, final size, and alignment from the facade layout

#### Scenario: Inspect a cross-module nominal dependency

- **WHEN** a field resolves through an imported namespace alias
- **THEN** the lab links its syntax, type lookup, canonical imported struct, and nested layout entry

#### Scenario: Inspect inline recursion

- **WHEN** a preset contains a direct or mutual inline struct cycle
- **THEN** the lab retains every declaration and dependency edge while marking the participating layouts unavailable with their canonical cause

#### Scenario: Inspect damaged fields

- **WHEN** a struct contains missing, duplicate, unknown, or inaccessible field data
- **THEN** the lab presents each retained field state and exact diagnostic while unrelated structs remain fully inspectable
