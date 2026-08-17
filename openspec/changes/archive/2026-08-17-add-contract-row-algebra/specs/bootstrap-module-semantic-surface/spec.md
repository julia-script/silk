## ADDED Requirements

### Requirement: Module surfaces encode row contracts deterministically

Exported callable surfaces SHALL encode symbolic row expressions, member-well-formedness obligation
keys, generic constraints, constrained-callable schemas, neutral `ProviderMatch` and
`WitnessIdentity` data, and exact-wanted-branded selection evidence wherever that evidence is part
of the semantic surface. Source origins SHALL be excluded from semantic equality and encoding
identity while remaining available as local diagnostic metadata.

Encoding and decoding SHALL preserve source/intrinsic witness origin, every specialized generic
argument of conditional witnesses, constraint grouping identity, and deterministic definitional
keys. Equivalent surfaces SHALL compare equal independently of declaration order or source offsets.

#### Scenario: Round-trip a constrained callable surface

- **WHEN** an exported callable carries `Without<R, S>` and a provider-selection obligation
- **THEN** encode/decode preserves binders, row expressions, constraint key, residual evidence, and callable identity exactly

#### Scenario: Distinguish specialized witness identities

- **WHEN** one conditional witness declaration specializes with two different generic argument lists
- **THEN** their encoded `WitnessIdentity` values remain distinct after a module-surface round trip

#### Scenario: Exclude locations from semantic surface identity

- **WHEN** equivalent constrained declarations differ only in source offsets
- **THEN** their semantic surface identity is equal while their local diagnostic origins remain distinct
