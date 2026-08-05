## Purpose

Deterministic discovery of the concrete runtime instances reachable from the user entry:
instance keys over canonical declaration identities, and the recorded worklist whose
record-before-follow discipline makes ordinary recursion terminate — the structure the full
generic language inherits, degenerate while the slice has no type or contract-row arguments.

## ADDED Requirements

### Requirement: Instances are discovered from the entry by a recorded worklist

Instance discovery SHALL start from the user entry — the root module's unique zero-parameter
`I32` `main` — and SHALL follow resolved calls in the entry's HIR transitively. The deterministic
worklist SHALL record an instance before following it, so directly and mutually recursive
programs terminate with each instance discovered exactly once, in deterministic discovery order.
Declarations of the closure that are not reachable from the entry SHALL NOT become instances.

#### Scenario: Discover a call chain once each

- **WHEN** `main` returns `identity(identity(42))`
- **THEN** discovery records exactly the `main` and `identity` instances in that order

#### Scenario: Terminate on recursion

- **WHEN** `main` returns `main()`
- **THEN** discovery records the `main` instance exactly once and terminates

#### Scenario: Exclude unreachable declarations

- **WHEN** the closure contains a declaration no reachable body calls
- **THEN** it produces no instance

### Requirement: Instance keys are canonical and normalized

An instance key SHALL consist of the canonical declaration identity plus normalized concrete
type and contract-row arguments — both empty in the frozen slice — and equal keys SHALL identify
the same instance.

#### Scenario: Key the degenerate slice

- **WHEN** any instance is discovered in the frozen slice
- **THEN** its key carries the canonical declaration identity with empty type and contract-row arguments

### Requirement: An unavailable entry stays explicit

When the root module has no unique valid entry — missing, ambiguous, parameterized, or not
returning resolved `I32` — discovery SHALL report an explicitly unavailable entry with its
reason and SHALL record no instances, rather than choosing a declaration or failing.

#### Scenario: Report a missing entry

- **WHEN** the root module declares no `main`
- **THEN** discovery reports an unavailable entry with a missing-entry reason and an empty instance list
