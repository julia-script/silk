## ADDED Requirements

### Requirement: Signed integer literal facts

A present integer literal with a directly applied minus sign SHALL produce a signed exact value
fact typed `I32` in its context. Literals SHALL be range-checked against the full signed `I32`
range: values above `2147483647` or below `-2147483648` SHALL keep the existing `SEM0002`
out-of-range diagnostic and an explicit out-of-range fact, and `-2147483648` itself SHALL be a
valid exact value.

#### Scenario: Analyze a negative literal

- **WHEN** a body returns `-42`
- **THEN** the integer fact carries the exact value `-42` typed `I32` with no diagnostics

#### Scenario: Accept the signed minimum

- **WHEN** a body returns `-2147483648`
- **THEN** the fact carries that exact value rather than an out-of-range state

#### Scenario: Reject one below the signed minimum

- **WHEN** a body returns `-2147483649`
- **THEN** the fact is out-of-range with one `SEM0002` diagnostic at the literal's span

### Requirement: Compiler-known actor operations resolve without source declarations

Qualified calls SHALL resolve against the compiler-known built-in actor table rather than source
declarations: the `I32` actor SHALL expose the ordinary trapping arithmetic operations `add`,
`subtract`, `multiply`, `divide`, and `remainder`, each accepting two `I32` arguments and
producing `I32`. Built-in operations MUST NOT appear in the declaration index, MUST NOT be
callable by bare name, and their argument facts SHALL follow the same recursive analysis and
arity checking as user calls, with a wrong arity keeping the expression unavailable. A qualified
call naming an unknown actor SHALL produce one `SEM0009` diagnostic, and a known actor with an
unknown operation SHALL produce one `SEM0010` diagnostic, each at the exact offending span with
the expression kept explicitly unavailable.

#### Scenario: Resolve a built-in arithmetic call

- **WHEN** a body returns `I32.add(40, 2)`
- **THEN** the call fact resolves to the built-in operation, both argument facts are exact values, the expression type is `I32`, and no diagnostics are produced

#### Scenario: Diagnose an unknown actor

- **WHEN** a body returns `Math.add(1, 2)`
- **THEN** one `SEM0009` diagnostic marks the actor identifier and the expression is explicitly unavailable

#### Scenario: Diagnose an unknown operation

- **WHEN** a body returns `I32.frobnicate(1, 2)`
- **THEN** one `SEM0010` diagnostic marks the operation identifier and the expression is explicitly unavailable

#### Scenario: Keep bare built-in names unresolved

- **WHEN** a body returns `add(1, 2)` with no such source declaration
- **THEN** the call keeps the existing unknown-function diagnostic rather than resolving to the built-in actor
