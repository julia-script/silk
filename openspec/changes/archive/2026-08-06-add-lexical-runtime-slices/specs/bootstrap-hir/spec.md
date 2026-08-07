## ADDED Requirements

### Requirement: HIR retains lexical slice semantics explicitly

HIR SHALL represent each available slice type, whole-root borrow or reborrow, loan identity, access
mode, backing-place provenance, runtime length projection, and borrowed indexed place without
encoding a raw address. Expression and place traversal SHALL preserve source evaluation order and
exact spans, and unavailable slice facts MUST NOT become typed HIR operations.

#### Scenario: Retain a shared whole-array borrow

- **WHEN** semantic analysis accepts `fold(&values)`
- **THEN** HIR records a shared slice formation tied to the array root and call region before the call argument

#### Scenario: Retain exclusive indexed replacement order

- **WHEN** an exclusive slice assignment has a dynamic index and an effectful replacement expression
- **THEN** HIR orders source-root resolution, index evaluation, runtime bounds validation, replacement evaluation, old-value cleanup, and committed write exactly once

#### Scenario: Omit an unavailable borrow operation

- **WHEN** borrow analysis lacks a stable source root or compatible slice destination
- **THEN** HIR preserves the diagnostic cause through surrounding unavailable facts and emits no executable borrow node
