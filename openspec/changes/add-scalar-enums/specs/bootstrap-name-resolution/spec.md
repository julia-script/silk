## ADDED Requirements

### Requirement: Qualified enum members resolve through canonical enum identity

Enum type names SHALL resolve through ordinary explicit module scopes and visibility boundaries.
After the qualifier resolves to an enum type, `EnumName.Member` SHALL resolve the member within that
enum's canonical member set rather than through module lookup or standard-library actor spelling.
Unknown, inaccessible, and wrong-enum members SHALL remain distinct deterministic resolution states.

#### Scenario: Resolve a visible imported enum member

- **WHEN** a module imports a public enum and refers to one of its qualified members
- **THEN** resolution records the imported enum identity and that member's canonical identity

#### Scenario: Reject an unknown enum member

- **WHEN** a qualified member path names no member declared by its resolved enum
- **THEN** resolution reports the dedicated unknown-enum-member diagnostic at the member span

#### Scenario: Reject a member through the wrong enum

- **WHEN** a member selected from one canonical enum is required to belong to another enum identity
- **THEN** analysis reports the dedicated wrong-enum-member diagnostic at the member path

#### Scenario: Reject unqualified member construction

- **WHEN** a bare identifier has the same spelling as an enum member but no ordinary binding declares it
- **THEN** resolution reports the ordinary unresolved-name state rather than searching visible enum member sets
