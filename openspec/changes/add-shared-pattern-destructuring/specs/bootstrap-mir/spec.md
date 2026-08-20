## ADDED Requirements

### Requirement: MIR lowers statement patterns as verified selections

MIR SHALL lower local and conditional patterns through the compiler-owned match operation rather
than a second dispatch mechanism. The operation SHALL retain the specialized logical scrutinee,
canonical selected members, access, binding locals, structured outcomes, cleanup, provenance, and
whether statement bindings remain live after selection. Verification SHALL reject inconsistent
member tests, locals, access, coverage, or retained-borrow state.

#### Scenario: Lower an irrefutable let

- **WHEN** HIR contains total nested destructuring
- **THEN** MIR selects once, creates the declared binding locals, and retains them in the enclosing region

#### Scenario: Lower if-let mismatch

- **WHEN** a conditional pattern is refutable
- **THEN** MIR contains one source-ordered selected body and one deterministic mismatch body with joined cleanup
