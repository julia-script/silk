## ADDED Requirements

### Requirement: A nominal union is one atomic structural-union member

A complete represented nominal union MAY be one ordinary member of `A | B`. Structural
normalization SHALL use the applied parent union type as that member's identity and SHALL NOT flatten
its variants, merge a variant with another structural member, or expose its private tag. Ordinary
contextual injection and widening SHALL first construct or preserve the nominal union value and then
map that whole value as one structural member.

#### Scenario: Inject a nominal variant into a structural union

- **WHEN** `HttpError.Dns { ... }` enters an expected `HttpError | OutOfMemoryError`
- **THEN** construction first produces `HttpError` and structural conversion injects that complete nominal value as one member

#### Scenario: Preserve the nominal boundary during normalization

- **WHEN** one `HttpError` variant has the same payload shape as another structural member
- **THEN** normalization retains `HttpError` and the other member as distinct types and does not flatten the matching variant

## MODIFIED Requirements

### Requirement: Pattern selection uses exact normalized union members

Pattern analysis SHALL select ordinary structural-union roots by canonical normalized type identity.
Nominal values, scalars, arrays, strings, and finite represented executable members MAY be exact
whole-member selectors when they are valid members of the scrutinee. When an exact root is a nominal
union, a qualified variant pattern MAY additionally select one subordinate canonical variant leaf.
That selection SHALL retain a canonical path containing the structural root, applied nominal parent,
and variant; it SHALL NOT turn the variant into a structural-union member, invent a second membership
relation, or expose either numeric runtime tag.

#### Scenario: Select a scalar member

- **WHEN** an `i32 | string` value is matched by an `i32 number` pattern
- **THEN** the selected binding has exact type `i32` and coverage removes that canonical member

#### Scenario: Reject a foreign exact member

- **WHEN** a pattern selects `bool` from `i32 | string`
- **THEN** analysis identifies `bool` as absent from the normalized scrutinee members

#### Scenario: Select a subordinate nominal variant

- **WHEN** `HttpError.Dns { .. }` matches through structural root `HttpError` in `HttpError | OutOfMemoryError`
- **THEN** selection retains the complete root-parent-variant path while the normalized structural member set remains exactly `HttpError | OutOfMemoryError`
