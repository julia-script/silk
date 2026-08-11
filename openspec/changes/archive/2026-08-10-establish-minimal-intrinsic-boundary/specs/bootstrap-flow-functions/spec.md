## ADDED Requirements

### Requirement: Effects use source-defined services generically

An Effect requirement MAY name any visible source-declared service and role. Requirement
normalization, service-slot shaping, witness dispatch, and `Effect.provide`, `Effect.provideMut`,
and acquisition-based provision SHALL operate from declaration and conformance facts rather than a
compiler-known capability list. `Effect.result` and requirement binding MAY remain source wrappers
over minimal `Intrinsic` machinery.

#### Scenario: Compose an arbitrary service requirement

- **WHEN** an Effect calling a user-declared service is mapped, tapped, stored, and provided
- **THEN** every combinator preserves or discharges the service requirement by the ordinary row rules

#### Scenario: Avoid a service-specific Effect intrinsic

- **WHEN** Logger or FileSystem is added after this change
- **THEN** no new Effect intrinsic, compiler recipe kind, or name-based lowering rule is required
