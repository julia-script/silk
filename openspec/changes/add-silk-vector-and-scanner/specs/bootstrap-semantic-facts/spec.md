## ADDED Requirements

### Requirement: Conformance facts bind impl type parameters

Semantic analysis SHALL publish conformance facts for parametric conformances in which the impl's
type parameters are bound across the capability, the target type, and the hook or operation
signatures. Analysis SHALL reject with precise deterministic diagnostics: a parameter never used by
the target type, a parameter name declared twice, and a reference to an undeclared parameter.

#### Scenario: Parameters bind across the conformance

- **WHEN** source declares `impl<T> Drop for Vector<T>` with hook parameter `self: &mut Vector<T>`
- **THEN** the published conformance fact resolves both `T` references to the same bound parameter and no unknown-type diagnostic is produced

#### Scenario: Reject an unbound impl parameter

- **WHEN** an impl declares a type parameter that its target type does not use
- **THEN** analysis reports a deterministic diagnostic naming the unused parameter and publishes no dispatchable conformance fact

#### Scenario: Reject Drop on always-Copy instantiations at monomorphization

- **WHEN** a parametric Drop conformance is instantiated at an element type making the whole provider Copy
- **THEN** the existing Copy-cannot-implement-Drop rejection fires for that instantiation with the instantiated type named in the diagnostic
