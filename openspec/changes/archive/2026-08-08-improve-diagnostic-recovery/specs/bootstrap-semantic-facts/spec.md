## RENAMED Requirements

- FROM: `Unknown parameter reference diagnostic`
- TO: `Unknown value reference diagnostic`

## MODIFIED Requirements

### Requirement: Unknown value reference diagnostic

A present bare identifier with no matching local parameter, preceding binding, or in-scope pattern
binding SHALL retain a `Missing` reference fact and produce one `SEM0006` diagnostic at the exact
reference span using value-name terminology. Duplicate declarations SHALL rely on
declaration-owned `SEM0005` diagnostics without adding a second ambiguity diagnostic at the
reference. Diagnostics SHALL remain deterministic and phase-separated with existing lexical,
parser, and semantic diagnostics.

#### Scenario: Diagnose an unknown value name

- **WHEN** a function returns `missing` without any in-scope value named `missing`
- **THEN** the reference is missing and one `SEM0006` diagnostic identifies the exact identifier span as an unknown value

#### Scenario: Avoid duplicate ambiguity diagnostics

- **WHEN** a reference matches duplicate parameter declarations
- **THEN** only the later declarations carry `SEM0005` and no reference-site ambiguity diagnostic is added

#### Scenario: Repeat value analysis

- **WHEN** equivalent value declarations and references are analyzed repeatedly in fresh processes
- **THEN** identities, lookup outcomes, reference facts, types, compatibility, and diagnostic ordering are identical

### Requirement: Mutation facts identify one writable place

Semantic analysis SHALL publish whether each binding is mutable and one ordered place fact for every
assignment destination, including its root binding, field/index selectors, dynamic checks, exact
destination type, right-hand type, compatibility, provenance, and complete-or-unavailable write
outcome. A failed selector SHALL leave earlier place facts queryable and make later steps causally
unavailable without choosing another destination. A destination already unavailable because of a
parser or name-resolution diagnostic SHALL retain that cause without adding a non-writable-place
diagnostic.

#### Scenario: Resolve a nested array write

- **WHEN** source assigns to `pairs[index].left`
- **THEN** facts identify the mutable array root, checked index, canonical field, exact `I32` destination, and assignment compatibility

#### Scenario: Diagnose a non-writable destination

- **WHEN** an assignment targets a resolved immutable binding or resolved non-place expression
- **THEN** the destination facts retain the exact root or expression and one stable non-writable diagnostic

#### Scenario: Preserve an unavailable destination cause

- **WHEN** an assignment destination is unavailable because its identifier is missing or its syntax is damaged
- **THEN** the write remains unavailable with the originating cause and no `SEM0036` diagnostic is added
