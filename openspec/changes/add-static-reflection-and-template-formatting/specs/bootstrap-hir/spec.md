## ADDED Requirements

### Requirement: Static reflection and iteration erase before runtime HIR publication

Residual HIR SHALL contain only the ordinary literals, aggregate field projections, interface-
selected calls, Writer operations, and control flow produced by successful static reflection and
iteration. It MUST NOT contain static-for nodes, type descriptors, field descriptors, static
sequences, template plans, reflection lookups, or runtime field selection by name or ordinal.
Generated operations SHALL retain both their authored body provenance and the static element or
template segment that caused their elaboration.

#### Scenario: Erase a heterogeneous static loop

- **WHEN** static iteration generates different `Display` calls for `string` and `i32` tuple fields
- **THEN** HIR contains two ordinary typed calls and projections in source order and no common runtime reflection operation
