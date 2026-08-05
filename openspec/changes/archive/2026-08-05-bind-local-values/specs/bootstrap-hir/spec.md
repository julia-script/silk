## MODIFIED Requirements

### Requirement: HIR is resolved, typed, and canonically identified

Every HIR function body SHALL be an ordered statement sequence: zero or more binding statements
followed by one return statement. Every HIR expression SHALL be a core semantic operation —
integer literal, parameter reference, binding reference, move, or call — carrying its resolved
type and exact source provenance. Calls SHALL reference their target's canonical declaration
identity, parameter references their function-local parameter identity, and binding references
and moves their function-local binding identity. Normalized function contracts (ordered
parameter types and result type) SHALL be published per declaration. An unknown fact SHALL
remain an explicit unavailable state carrying the originating diagnostic's identity where one
exists, and MUST NOT masquerade as a valid empty contract, resolved reference, or concrete type.

#### Scenario: Reference a call target canonically

- **WHEN** `main` returns `answer()` and `answer` is a present unique declaration
- **THEN** the HIR call references `answer`'s canonical identity and carries the resolved `I32` type

#### Scenario: Keep unknown facts explicit

- **WHEN** a body references an unknown function or an unknown parameter
- **THEN** the corresponding HIR expression is an explicit unavailable state carrying the originating diagnostic's identity, and the enclosing contract or type is not defaulted

#### Scenario: Normalize function contracts

- **WHEN** a declaration has two resolved `I32` parameters and a resolved `I32` return
- **THEN** its published contract lists both parameter types in order and the result type, while any unresolved header type keeps the whole contract explicitly unavailable

#### Scenario: Elaborate a binding sequence

- **WHEN** a body spells `let value = identity(42) return value`
- **THEN** the HIR body is one binding statement whose initializer is a typed call followed by one return whose expression is a typed binding reference to that binding

#### Scenario: Keep a damaged statement explicit

- **WHEN** one binding statement's initializer contains an unresolved reference
- **THEN** that initializer is an explicit unavailable expression carrying the originating diagnostic's identity while the statement sequence and the other statements' facts remain intact
