## MODIFIED Requirements

### Requirement: Structural unions normalize to finite ordinary member sets

A value union SHALL be an unordered, duplicate-free set of canonical detached ordinary value
types. Nested unions SHALL flatten, spelling order and duplicate members SHALL not affect identity,
a one-member union SHALL normalize to that member, and `never` SHALL denote the empty uninhabited
union. Transparent alias spelling SHALL NOT affect normalized identity: a member that names an
alias contributes the alias's erased target, and a union alias contributes each of its members.
Scalars, arrays, `string`, nominal values, and exact or opaque executable values with a finite
representation MAY be direct members. Lexical borrows and bare callable or Effect contracts without
standalone storage SHALL NOT be direct members.

#### Scenario: Normalize order nesting and duplicates

- **WHEN** equivalent type positions spell `Token | i32`, `i32 | Token`, and `Token | (i32 | Token)`
- **THEN** all three produce one canonical type with the same deterministically ordered members

#### Scenario: Normalize the empty and singleton cases

- **WHEN** normalization receives `never | i32` or `i32 | i32`
- **THEN** each normalizes to the precise ordinary `i32` type

#### Scenario: Normalize through an alias

- **WHEN** `type Pair = Token | i32` is declared and a type position spells `Pair | Fault`
- **THEN** it produces the same canonical type as `Token | i32 | Fault`

#### Scenario: Reject a non-nominal member

- **WHEN** a union type directly includes a lexical borrow or bare executable contract without finite representation
- **THEN** analysis reports the exact invalid member and publishes no available union type

#### Scenario: Admit represented executable members

- **WHEN** a union includes an exact callable or opaque Effect value with a finite capture representation
- **THEN** the union retains its public contract and compiler-private represented member identity
