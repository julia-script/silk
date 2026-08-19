## ADDED Requirements

### Requirement: Copy is one sealed validated property

A type SHALL be Copy only through the compiler's single sealed Copy property. A user MAY declare `impl Copy` without operations when every stored field is Copy and no cleanup obligation exists. The compiler SHALL reject operation bodies, non-Copy fields, `Drop`, allocation ownership, and conflicting evidence.

#### Scenario: Opt a plain struct into Copy

- **WHEN** a struct containing only Copy fields declares an empty `impl Copy`
- **THEN** reads may duplicate its value and arrays, unions, and generic bounds derive that same property

#### Scenario: Reject Copy over allocated storage

- **WHEN** a struct owns allocated memory or has a Drop hook and declares `impl Copy`
- **THEN** conformance validation rejects the declaration before ownership analysis uses it

### Requirement: Stored executable values obey ordinary aggregate ownership

Represented callable and Effect values SHALL derive Copy, moves, partial moves, cleanup, and storage behavior from their realized fields. The compiler SHALL retain access-specific capture restrictions but SHALL NOT classify every executable-bearing nominal as move-only solely because it contains executable representation.

#### Scenario: Store a Copy callable representation

- **WHEN** a callable representation contains only Copy captures and satisfies the sealed Copy rule
- **THEN** an aggregate containing it follows ordinary Copy behavior

#### Scenario: Move one affine executable field

- **WHEN** an aggregate contains an affine captured callable and another field
- **THEN** moving the callable field uses ordinary partial-move rules and cleanup skips exactly that moved field
