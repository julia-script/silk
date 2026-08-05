## ADDED Requirements

### Requirement: Arithmetic evaluates exactly and traps on the pinned conditions

The interpreter SHALL execute binary operations with exact signed 32-bit results. Signed
overflow, division by zero, and `-2147483648` divided or remaindered by `-1` SHALL produce a
`Blocked` trap outcome carrying the operation's function identity, an arithmetic reason, and the
operation's provenance — never a wrapped or guessed value. Division SHALL truncate toward zero
and remainder SHALL take the dividend's sign. These outcomes SHALL agree with native execution
across the corpus: matching results for completing programs and matching abnormal termination for
trapping programs.

#### Scenario: Evaluate arithmetic exactly

- **WHEN** `main` returns `I32.subtract(I32.multiply(6, 7), 0)`
- **THEN** evaluation completes with exact result `42`

#### Scenario: Trap on signed overflow

- **WHEN** `main` returns `I32.add(2147483647, 1)`
- **THEN** evaluation blocks at that operation with an arithmetic trap reason and its provenance

#### Scenario: Trap on division by zero

- **WHEN** `main` returns `I32.divide(1, 0)`
- **THEN** evaluation blocks at that operation rather than producing a value

#### Scenario: Truncate division toward zero

- **WHEN** `main` returns `I32.divide(-7, 2)`
- **THEN** evaluation completes with exact result `-3`
