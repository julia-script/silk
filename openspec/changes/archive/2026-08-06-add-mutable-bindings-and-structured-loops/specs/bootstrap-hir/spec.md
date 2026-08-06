## ADDED Requirements

### Requirement: HIR represents typed mutation explicitly

HIR SHALL distinguish immutable and mutable bindings and SHALL represent each accepted assignment as
one typed write to an ordered binding/field/index place with exact root identity, selector types,
right-hand value, replacement mode, and provenance. It MUST NOT desugar a write into a fabricated
setter call or a partial aggregate value.

#### Scenario: Elaborate an indexed write

- **WHEN** semantic facts accept `values[index] = next`
- **THEN** HIR contains one checked typed write rooted at the mutable array owner

### Requirement: HIR control structure is an acyclic region graph

HIR SHALL represent `while` as one structured loop region containing its condition and body regions,
and SHALL represent `break` and `continue` as lexical region outcomes targeting the canonical
enclosing loop. Child, sequencing, and continuation relationships SHALL form a DAG; repetition is a
property of the loop region and MUST NOT appear as a cyclic HIR edge.

#### Scenario: Elaborate a nested loop DAG

- **WHEN** a function contains nested loops, conditionals, `break`, and `continue`
- **THEN** HIR retains canonical nested regions and lexical outcomes in an acyclic deterministic traversal order
