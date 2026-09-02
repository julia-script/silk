## ADDED Requirements

### Requirement: The module basename has no semantic role in qualified lookup

A qualifier that resolves to a nominal declaration SHALL expose only that declaration's associated
items. A qualifier that resolves to a module namespace SHALL expose only that module's public root
declarations. Name resolution MUST NOT compare a declaration name with its module's basename, MUST
NOT project root declarations through a type, and MUST NOT project inherent members through a
namespace. An unknown member under a nominal qualifier SHALL report an unknown associated member;
an unknown member under a namespace SHALL report an unknown module member.

#### Scenario: A basename match exposes nothing

- **WHEN** `counter.silk` declares `pub struct Counter` and a root `pub fn increment` with no `impl Counter`
- **THEN** `Counter.increment(...)` reports an unknown associated member and `import counter { increment }` resolves the root function

#### Scenario: A namespace does not expose members

- **WHEN** a module imports `silk.option as OptionModule` and calls `OptionModule.map(...)`
- **THEN** resolution reports an unknown module member because `map` is an inherent member of `Option`, not a root declaration

#### Scenario: Type import is not a namespace

- **WHEN** a module imports `silk.vector { Vector }` and `vector.silk` also declares a root `pub fn debugDump`
- **THEN** `Vector.debugDump()` reports an unknown associated member and `Vector.append(...)` resolves the inherent member

#### Scenario: Completion and resolution agree

- **WHEN** completion is requested after `Counter.` in the basename-matching module above
- **THEN** no root function is offered
