## ADDED Requirements

### Requirement: Name resolution is a facade query

The facade SHALL expose each module's immutable scope, import-binding outcomes, collisions, and
unqualified or qualified declaration lookup facts from the same snapshot used for HIR, ownership,
instance discovery, MIR, evaluation, and codegen. Tooling consumers MUST NOT reconstruct module
scopes from syntax or declaration headers. Damaged imports and failed lookups SHALL retain their
explicit unavailable state and originating diagnostic cause.

#### Scenario: Query a hybrid import scope

- **WHEN** a snapshot module imports `compiler.Syntax as Tree { parse }`
- **THEN** the facade reports namespace binding `Tree`, selected binding `parse`, and their canonical target module and declaration identities

#### Scenario: Query a cross-module call reference

- **WHEN** a body calls a public function through a valid namespace alias
- **THEN** the facade's lookup fact and HIR query identify the same canonical imported declaration

#### Scenario: Query a binding collision

- **WHEN** a local declaration and selected import claim the same module-scope spelling
- **THEN** the facade exposes every conflicting binding, the unavailable lookup, and its diagnostic cause without choosing a winner

#### Scenario: Query around a damaged import

- **WHEN** one import clause contains recovered syntax
- **THEN** its unavailable binding facts remain queryable while unrelated module scopes, HIR, and declarations answer completely
