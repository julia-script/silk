## ADDED Requirements

### Requirement: Anonymous expressions create statically identified callable values

Every accepted anonymous callable expression SHALL create one exact callable value whose executable
target is the source occurrence of its body and whose finite environment contains its implicit
captures in first-reference source order. The target identity SHALL be deterministic within its
canonical enclosing executable and SHALL remain distinct from every other occurrence, including
textually identical bodies. An anonymous callable SHALL inherit surrounding generic substitutions
but MUST NOT declare independent type parameters, participate in overload lookup, expose an
importable declaration name, or use a universal erased closure representation. A capture-free
anonymous callable SHALL use the same exact model with an empty environment.

#### Scenario: Distinguish identical occurrences

- **WHEN** one body contains two textually identical capture-free anonymous callables
- **THEN** each value retains a different deterministic source-occurrence target while repeated analysis produces the same identities

#### Scenario: Preserve outer substitution

- **WHEN** an anonymous contract refers to an enclosing function's type parameter
- **THEN** each enclosing specialization produces the corresponding finite anonymous target and environment without an independently generic declaration

### Requirement: Anonymous contracts compose as ordinary callable contracts

An ordinary anonymous body SHALL produce its explicitly declared result when invoked. Invoking an
effectful anonymous body SHALL construct the declared lazy Effect value, preserving its success,
failure, and requirement channels until `run`. The derived `fn`, `mut fn`, or `once fn` mode SHALL be
the callable's strongest contract: shared anonymous values SHALL satisfy exclusive or consuming
requirements, and exclusive anonymous values SHALL satisfy consuming requirements, while reverse
substitutions MUST be rejected. Named function items, automatic sections, bound methods, and stored
callables SHALL retain their existing contracts and identities.

#### Scenario: Use an inline recovery handler

- **WHEN** `Effect.catchAll` receives `effect fn(error: Failure) -> i32 { return 42 }`
- **THEN** invocation constructs the declared recovery Effect and running it can complete with `42` without a named handler declaration

#### Scenario: Accept a shared anonymous value once

- **WHEN** a capture-free anonymous callable is passed where `once fn(i32) -> i32` is required
- **THEN** the existing stronger-to-weaker callable substitution accepts it
