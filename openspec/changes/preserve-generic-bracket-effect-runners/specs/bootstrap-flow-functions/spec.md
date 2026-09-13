## MODIFIED Requirements

### Requirement: Effects use source-defined services generically

An Effect requirement MAY name any visible source-declared service and role. Requirement
normalization, service-slot shaping, witness dispatch, and `Effect.provide`, `Effect.provideMut`,
and acquisition-based provision SHALL operate from declaration and conformance facts rather than a
compiler-known capability list. Provision through a generic provider SHALL remain valid when
ordinary ownership and pattern selection place the provider behind an owned aggregate or active
nominal-union member. `Effect.result` and requirement binding MAY remain source wrappers over
minimal `Intrinsic` machinery.

#### Scenario: Compose an arbitrary service requirement

- **WHEN** an Effect calling a user-declared service is mapped, tapped, stored, and provided
- **THEN** every combinator preserves or discharges the service requirement by the ordinary row rules

#### Scenario: Provide through an owned generic bracket guard

- **WHEN** an ordinary generic bracket callback selects an owned provider from an active nominal-union member and provides it to a user-declared service operation
- **THEN** analysis and execution use the selected conformance with the same exact service result and cleanup behavior as a direct generic provider field

#### Scenario: Avoid a service-specific Effect intrinsic

- **WHEN** Logger or FileSystem is added after this change
- **THEN** no new Effect intrinsic, compiler recipe kind, or name-based lowering rule is required
