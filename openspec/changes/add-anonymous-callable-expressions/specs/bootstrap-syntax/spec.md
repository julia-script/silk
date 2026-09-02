## ADDED Requirements

### Requirement: Anonymous callable expressions are lossless and locally recoverable

Expression positions SHALL accept `fn(parameters) -> Result { statements }` and
`effect fn(parameters) -> Success ! Failure ? Requirements { statements }` as dedicated anonymous
callable expressions. The concrete tree SHALL retain the optional `effect`, `fn`, every parameter,
explicit result, optional failure and requirement rows, body, punctuation, trivia, and exact spans.
Anonymous construction MUST NOT accept a name, type-parameter list, declaration modifier, or
explicit `mut` or `once` invocation modifier. `effect { ... }` SHALL remain the distinct lazy Effect
expression. Missing signature or body parts SHALL become explicit local recovery data, parsing SHALL
make bounded progress, and later arguments, statements, and declarations SHALL remain recoverable.

#### Scenario: Parse ordinary and effectful anonymous callables

- **WHEN** expressions spell `fn(value: i32) -> i32 { return value }` and `effect fn(error: Failure) -> i32 ! Failure { return 42 }`
- **THEN** the syntax tree contains distinct complete anonymous callable nodes reproducing every source token exactly once

#### Scenario: Keep Effect construction distinct

- **WHEN** one expression starts `effect {` and another starts `effect fn(`
- **THEN** the first remains an Effect block and the second is an effectful anonymous callable

#### Scenario: Recover a damaged anonymous signature

- **WHEN** an anonymous callable omits a parameter type, arrow result, delimiter, or body before a following call argument or declaration
- **THEN** the damage remains inside the anonymous node with stable diagnostics and parsing resumes without non-termination

