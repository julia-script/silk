## MODIFIED Requirements

### Requirement: Instances are discovered from the entry by a recorded worklist

Instance discovery SHALL start from the user entry — the root module's unique zero-parameter
public `I32` `main` — and SHALL follow resolved local and cross-module calls in HIR transitively.
The deterministic worklist SHALL record an instance before following it, so directly and mutually
recursive programs terminate with each canonical instance discovered exactly once, in deterministic
discovery order. Declarations of the closure that are not reachable from the entry SHALL NOT become
instances, whether or not their modules are imported.

#### Scenario: Discover a call chain once each

- **WHEN** `main` returns `identity(identity(42))`
- **THEN** discovery records exactly the `main` and `identity` instances in that order

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
