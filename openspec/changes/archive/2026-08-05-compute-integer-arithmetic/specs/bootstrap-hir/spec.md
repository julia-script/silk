## ADDED Requirements

### Requirement: Built-in calls are typed HIR operations

HIR SHALL represent a resolved built-in actor call as a dedicated builtin-call expression
carrying the closed operation name (`Add`, `Subtract`, `Multiply`, `Divide`, or `Remainder`), its
typed argument expressions in order, the resolved `I32` type, and exact source provenance.
Integer-literal expressions SHALL carry signed exact values. An unresolved actor, operation, or
argument SHALL keep the enclosing expression an explicit unavailable state carrying the
originating diagnostic's identity where one exists. The HIR encoder SHALL cover builtin calls and
signed values, gated by committed golden files.

#### Scenario: Elaborate a built-in call

- **WHEN** `pub fn main() -> I32 { return I32.add(40, 2) }` is elaborated
- **THEN** the returned expression is a builtin call with operation `Add`, two typed literal arguments, and type `I32`

#### Scenario: Elaborate a signed literal

- **WHEN** a body returns `-42`
- **THEN** the HIR literal carries the exact value `-42` typed `I32`

#### Scenario: Keep an unknown actor explicit

- **WHEN** a body returns `Math.add(1, 2)`
- **THEN** the HIR expression is an explicit unavailable state carrying the `SEM0009` diagnostic's identity
