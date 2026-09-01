## ADDED Requirements

### Requirement: Ordered call argument facts

Every call expression SHALL publish one ordered argument fact for every concrete argument. Each
argument fact SHALL have a zero-based ordinal, retain exact argument syntax provenance, and expose
the existing integer or local-parameter-reference expression fact and type state. Missing or damaged
argument syntax SHALL remain unavailable without creating a semantic argument or duplicating parser
diagnostics.

#### Scenario: Collect a literal argument

- **WHEN** `main` returns `identity(42)`
- **THEN** the call has one argument fact at ordinal zero with exact value `42`, type `I32`, and provenance to the literal syntax

#### Scenario: Collect a parameter-reference argument

- **WHEN** a function calls `identity(value)` using its resolved local parameter
- **THEN** the call's first argument retains that parameter reference and its available `I32` type

#### Scenario: Preserve argument source order

- **WHEN** a call contains two concrete arguments
- **THEN** its two argument facts have ordinals zero and one matching concrete list order

#### Scenario: Preserve parser ownership for a damaged argument

- **WHEN** argument syntax is missing or retained in an error region
- **THEN** no semantic argument is invented and the parser diagnostic remains the owning error

### Requirement: First positional call contract

A call whose function reference resolves uniquely SHALL map argument ordinal `n` to target parameter
ordinal `n`. Its call-contract fact SHALL be `Compatible` only when argument count equals parameter
count and every mapped argument and parameter type is available and equal. It SHALL be
`ArityMismatch` when the counts differ and `Unavailable` when the target is missing, ambiguous, or
syntax-unavailable or when any mapped type is unresolved or unavailable. Every mapped pair SHALL
retain the exact argument and target-parameter identities and syntax provenance.

#### Scenario: Bind one compatible argument

- **WHEN** `identity(value: I32)` is called as `identity(42)`
- **THEN** argument zero maps to parameter zero and the call contract is compatible

#### Scenario: Bind two arguments positionally

- **WHEN** a uniquely resolved two-parameter function is called with two available `I32` arguments
- **THEN** each argument maps to the parameter with the same ordinal and the call contract is compatible

#### Scenario: Preserve too few arguments

- **WHEN** a two-parameter target is called with one argument
- **THEN** the call contract is an arity mismatch with expected count two and actual count one

#### Scenario: Preserve too many arguments

- **WHEN** a one-parameter target is called with two arguments
- **THEN** the call contract is an arity mismatch with expected count one and actual count two

#### Scenario: Withhold a contract for an unavailable type

- **WHEN** a mapped parameter or argument type is unresolved or unavailable
- **THEN** the mapping remains visible but the call contract is unavailable

#### Scenario: Withhold a contract for an unresolved call

- **WHEN** top-level call resolution is missing, ambiguous, or syntax-unavailable
- **THEN** no target parameters are selected and the call contract is unavailable

### Requirement: Wrong call arity diagnostic

Every uniquely resolved call with a different argument and parameter count SHALL produce one
`SEM0007` diagnostic at the complete call span. Its reason data SHALL retain the target declaration
identity and expected and actual counts. Type-unavailable and unresolved calls SHALL not add an
arity or type diagnostic, and the existing return-type compatibility fact SHALL remain independent
from this call-contract fact.

#### Scenario: Diagnose too few arguments

- **WHEN** a one-parameter function is called with zero arguments
- **THEN** `SEM0007` covers the call and reports expected one and actual zero

#### Scenario: Diagnose too many arguments

- **WHEN** a zero-parameter function is called with one argument
- **THEN** `SEM0007` covers the call and reports expected zero and actual one

#### Scenario: Avoid cascading diagnostics

- **WHEN** a call target or mapped type is unavailable
- **THEN** the call contract is unavailable without adding `SEM0007` or a speculative type-mismatch diagnostic

#### Scenario: Repeat call-contract analysis

- **WHEN** equivalent calls and declarations are analyzed repeatedly in fresh processes
- **THEN** argument ordinals, mappings, compatibility states, reason data, and diagnostics are identical
