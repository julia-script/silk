## ADDED Requirements

### Requirement: Every executable body satisfies its resolved return contract before lowering

Semantic analysis SHALL prove that every explicit return, fallthrough path, and final expression of an ordinary function, Effect function, generic declaration, and conformance operation is compatible with the declaration's resolved return type. An `Effect<A>` value SHALL NOT satisfy an `A` return merely because the surrounding function is effectful.

#### Scenario: Reject a nested Effect at its return

- **WHEN** a body declared to return `i32` returns a call whose value is `Effect<i32>`
- **THEN** analysis reports a return-type mismatch at that expression and constructs no executable HIR or MIR body for the declaration

#### Scenario: Accept an explicitly nested Effect

- **WHEN** a body declared to return `Effect<i32>` returns a call whose value is `Effect<i32>`
- **THEN** analysis accepts the return without running or flattening the value

### Requirement: Invalid reachable bodies stop at the semantic boundary

A declaration with an unresolved or invalid executable body SHALL be unavailable to reachability and lowering. Calls through an interface witness SHALL preserve that same validity requirement rather than substituting an invalid mapped body into MIR.

#### Scenario: Reject issue 226 before the backend

- **WHEN** an interface-dispatched operation implementation violates its resolved return contract
- **THEN** the compiler emits the source semantic diagnostic and neither MIR verification nor a backend reports the primary failure
