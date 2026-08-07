## ADDED Requirements

### Requirement: HIR retains explicit flow and typed-failure semantics

HIR SHALL retain flow construction, capture access, one-layer run, failure origin, propagation, and
exact-member catch with normalized contracts and exact provenance. It MUST NOT contain exception,
unwinding, LLVM, Wasm, or runtime type-lookup vocabulary.

#### Scenario: Inspect a recovered flow

- **WHEN** a statically known handler recovers one nominal member
- **THEN** HIR shows the protected and handler targets, selected canonical member, residual row, and one run
