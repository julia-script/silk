## ADDED Requirements

### Requirement: MIR carries canonical logical union types

MIR SHALL represent a union as its normalized ordered nominal member set while referencing the
compiler-selected layout and calling shape for physical facts. Locals, contracts, struct fields,
arrays, writes, calls, returns, and drops SHALL preserve that logical type. MIR MUST NOT contain
source spelling order, aliases, backend types, numeric tags chosen outside the layout plan, or
backend-local control labels.

#### Scenario: Lower an aggregate-contained union

- **WHEN** HIR constructs and transports a struct whose field is `Token | End`
- **THEN** MIR retains one canonical logical union type and the program's matching layout-plan entry

### Requirement: MIR union conversion carries a total member mapping

MIR SHALL lower nominal injection and union widening to an explicit verified conversion operation
containing source and destination locals, exact source and target logical types, a total canonical
source-member to target-member mapping, layout/calling-shape references, access mode, and provenance.
The verifier SHALL reject unsorted or duplicate members, non-containing targets, incomplete or
incorrect mappings, inconsistent locals or layouts, and conversions that would narrow.

#### Scenario: Lower nominal injection

- **WHEN** HIR injects `Token` into `Token | End`
- **THEN** MIR contains one conversion mapping `Token` to its compiler-planned target member

#### Scenario: Lower union widening

- **WHEN** HIR widens `Token | End` to `Token | End | Fault`
- **THEN** MIR maps every source member exactly once while preserving the structured control DAG

#### Scenario: Reject an incomplete widening map

- **WHEN** hand-built MIR omits the `End` mapping from a two-member source union
- **THEN** verification reports the exact missing member before evaluation or emission

### Requirement: Union MIR encoding is deterministic

Text encoding SHALL include canonical union types, members, conversion mappings, layout references,
active-member cleanup plans, and provenance in stable order. Equivalent programs SHALL produce
byte-identical MIR across fresh processes without materializing mutable graph identity.

#### Scenario: Repeat a widening encoding

- **WHEN** one program injects, stores, widens, and drops a union in repeated fresh compilations
- **THEN** its MIR type keys, mappings, regions, cleanup, and textual bytes are identical
