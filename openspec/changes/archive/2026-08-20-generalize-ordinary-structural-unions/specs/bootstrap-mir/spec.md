## MODIFIED Requirements

### Requirement: MIR carries canonical logical union types

MIR SHALL represent a union as its normalized ordered ordinary member set while referencing the
compiler-selected layout and calling shape for physical facts. Locals, contracts, struct fields,
arrays, writes, calls, returns, and drops SHALL preserve that logical type. MIR MUST NOT contain
source spelling order, aliases, backend types, numeric tags chosen outside the layout plan, or
backend-local control labels.

#### Scenario: Lower an aggregate-contained union

- **WHEN** HIR constructs and transports a struct whose field is `i32 | Array<i32, 2> | Token`
- **THEN** MIR retains one canonical logical union type and the program's matching layout-plan entry

### Requirement: MIR union conversion carries a total member mapping

MIR SHALL lower ordinary-member injection and union widening to an explicit verified conversion
operation containing source and destination locals, exact represented source and target logical
types, a total canonical source-member to target-member mapping, layout/calling-shape references,
access mode, and provenance. The verifier SHALL reject unsorted or duplicate members,
non-containing targets, incomplete or incorrect mappings, inconsistent locals or layouts, and
conversions that would narrow.

#### Scenario: Lower nominal injection

- **WHEN** HIR injects an ordinary value such as `i32` or `Token` into `i32 | Token`
- **THEN** MIR contains one conversion mapping its exact source type to the compiler-planned target member

#### Scenario: Lower represented executable injection

- **WHEN** HIR injects an exact callable or opaque Effect value into a compatible union
- **THEN** MIR preserves its finite representation plan and maps it to the public canonical member

#### Scenario: Lower union widening

- **WHEN** HIR widens `i32 | Token` to `i32 | Token | Fault`
- **THEN** MIR maps every source member exactly once while preserving the structured control DAG

#### Scenario: Reject an incomplete widening map

- **WHEN** malformed MIR omits or duplicates one source member mapping
- **THEN** verification rejects the conversion before evaluation or backend emission
