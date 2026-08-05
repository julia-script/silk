## ADDED Requirements

### Requirement: Bool values and comparisons elaborate with exact types

`true` and `false` SHALL produce exact boolean value facts typed `Bool`. The built-in `I32`
actor SHALL additionally expose the comparison operations `equals`, `notEquals`, `lessThan`,
`lessOrEqual`, `greaterThan`, and `greaterOrEqual` — two `I32` arguments producing `Bool` — and
a built-in `Bool` actor SHALL expose `not` with one `Bool` argument producing `Bool`. Built-in
operations SHALL carry per-operation contracts (parameter types, result type, arity) and
resolution SHALL keep the existing `SEM0009`/`SEM0010` diagnostics for unknown actors and
operations.

#### Scenario: Elaborate a comparison

- **WHEN** a body returns `I32.lessThan(1, 2)`
- **THEN** the call resolves to the built-in comparison with expression type `Bool` and no diagnostics

#### Scenario: Elaborate boolean negation

- **WHEN** a body returns `Bool.not(true)`
- **THEN** the call resolves with one boolean argument and expression type `Bool`

### Requirement: Conditions and arguments are type-checked

The condition of a conditional statement SHALL elaborate to type `Bool`; a present condition of
any other available type SHALL produce one `SEM0011` diagnostic at the condition's span, with no
truthiness or coercion, and the conditional's arms still elaborated. A call argument mapped to a
parameter of a known different type — user or built-in — SHALL produce one `SEM0012` diagnostic
at the argument's span and keep the call expression explicitly unavailable.

#### Scenario: Reject an integer condition

- **WHEN** a body spells `if 1 { return 1 } return 0`
- **THEN** one `SEM0011` diagnostic marks the condition span and the arm's facts remain published

#### Scenario: Reject a boolean argument to arithmetic

- **WHEN** a body returns `I32.add(true, 1)`
- **THEN** one `SEM0012` diagnostic marks the first argument and the call expression is explicitly unavailable

#### Scenario: Reject a mistyped user call argument

- **WHEN** `identity(value: Bool)` is called with `identity(42)`
- **THEN** one `SEM0012` diagnostic marks the argument and the call expression is explicitly unavailable

#### Scenario: Check the return statement against a Bool contract

- **WHEN** `pub fn flag() -> Bool { return true }` and `pub fn broken() -> Bool { return 1 }` are elaborated
- **THEN** `flag`'s return compatibility is compatible and `broken`'s is unavailable
