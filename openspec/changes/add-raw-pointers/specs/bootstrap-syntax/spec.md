## ADDED Requirements

### Requirement: Raw pointer type syntax is lossless and recoverable

The parser SHALL recognize `*const <type>` and `*mut <type>` as one pointer type node in every
type position, retaining the star, the `const` or `mut` keyword, and the pointee type. A star not
followed by `const` or `mut` in type position SHALL report one missing-mutability diagnostic and
recover at the following type.

#### Scenario: Parse nested pointer types

- **WHEN** a parameter is typed `*mut *const u8`
- **THEN** the result contains a mutable pointer type node whose pointee is a constant pointer type node whose pointee is `u8`

#### Scenario: Recover from a bare star

- **WHEN** a parameter is typed `*u8`
- **THEN** the parser reports the missing-mutability diagnostic and the parameter still resolves its pointee type `u8`
