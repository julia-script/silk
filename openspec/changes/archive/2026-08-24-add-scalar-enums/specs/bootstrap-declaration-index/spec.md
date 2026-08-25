## ADDED Requirements

### Requirement: Scalar enums join the canonical nominal declaration index

The declaration index SHALL assign every enum one canonical nominal identity in the ordinary module
type namespace and SHALL publish its visibility, representation state, ordered members,
discriminants, and source provenance. Invalid or recovered enum facts SHALL remain explicit and SHALL
NOT prevent unrelated declarations or valid sibling members from being indexed.

#### Scenario: Index a visible enum and ordered members

- **WHEN** a public enum contains three valid members
- **THEN** the index exposes one public canonical enum identity and all three member facts in declaration order

#### Scenario: Preserve facts around an invalid discriminant

- **WHEN** one member has an out-of-range discriminant
- **THEN** its dependent value state is unavailable while the enum identity, valid sibling members, and unrelated declarations remain queryable
