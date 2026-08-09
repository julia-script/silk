## MODIFIED Requirements

### Requirement: Instances are discovered from the entry by a recorded worklist

Instance discovery SHALL start from one of the root module's two valid user entries: a unique
zero-parameter public ordinary `main() -> I32`, or a unique zero-parameter public
`effect fn main() -> Unit ! E` whose requirement row is empty and whose failure members all conform
to `Report`. Discovery SHALL retain the selected entry kind and normalized failure metadata and
SHALL follow resolved local and cross-module calls in HIR transitively. The deterministic worklist
SHALL record an instance before following it, so directly and mutually recursive programs terminate
with each canonical instance discovered exactly once, in deterministic discovery order.
Declarations of the closure that are not reachable from the entry SHALL NOT become instances,
whether or not their modules are imported.

#### Scenario: Discover a call chain once each

- **WHEN** ordinary `main` returns `identity(identity(42))`
- **THEN** discovery records exactly the `main` and `identity` instances in that order

#### Scenario: Discover an effectful entry chain

- **WHEN** effectful `main` runs one reachable effect function and can fail with one reportable type
- **THEN** discovery records `main`, the reachable function, the failure runtime type, and its cleanup hooks deterministically

#### Scenario: Discover a cross-module call chain

- **WHEN** root `main` calls a selectively imported public function which calls a function in a third module
- **THEN** discovery records all three instances once under their canonical module-qualified keys in call-discovery order

#### Scenario: Terminate on recursion

- **WHEN** `main` returns `main()`
- **THEN** discovery records the `main` instance exactly once and terminates

#### Scenario: Terminate on cross-module mutual recursion

- **WHEN** two imported public functions call one another and one is reachable from `main`
- **THEN** discovery records each canonical instance exactly once and terminates

#### Scenario: Exclude unreachable declarations

- **WHEN** the closure contains a declaration no reachable body calls
- **THEN** it produces no instance

### Requirement: An unavailable entry stays explicit

When the root module has no unique valid entry — missing, ambiguous, generic, parameterized,
ordinary with a non-`I32` result, effectful with a non-`Unit` result, effectful with unresolved
requirements, or effectful with an unreportable failure — discovery SHALL report an explicitly
unavailable entry with its reason and SHALL record no instances, rather than choosing a declaration
or failing.

#### Scenario: Report a missing entry

- **WHEN** the root module declares no `main`
- **THEN** discovery reports an unavailable entry with a missing-entry reason and an empty instance list

#### Scenario: Report an unreportable effect entry

- **WHEN** the root module's effectful `main` has a failure without `Report` conformance
- **THEN** discovery reports an unavailable entry with an unreportable-failure reason and an empty instance list

#### Scenario: Report an open effect entry

- **WHEN** the root module's effectful `main` retains a capability requirement
- **THEN** discovery reports an unavailable entry with an unresolved-requirements reason and an empty instance list
