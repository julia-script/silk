## MODIFIED Requirements

### Requirement: Conformance facts bind impl type parameters

Semantic analysis SHALL publish conformance facts for parametric conformances in which the impl's
type parameters are bound across the capability, the target type, and the hook or operation
signatures. Every parameter SHALL occur in the complete capability/provider head. Analysis SHALL
reject with precise deterministic diagnostics a parameter absent from both sides of that head,
a parameter name declared twice, and a reference to an undeclared parameter. Conditional bounds
SHALL retain their existing provider-subterm termination and complete-head coherence checks.

#### Scenario: Parameters bind across the conformance

- **WHEN** source declares `impl<T> Drop for Vector<T>` with hook parameter `self: &mut Vector<T>`
- **THEN** the published conformance fact resolves both `T` references to the same bound parameter and no unknown-type diagnostic is produced

#### Scenario: Interface arguments determine one reusable provider's witness

- **WHEN** source declares `impl<T> Echo<T> for Client` and selects `Echo<i32>` and `Echo<bool>` for the same concrete provider
- **THEN** both applications select the conformance with their own exact type arguments without adding phantom type parameters to `Client`

#### Scenario: Reject an unbound impl parameter

- **WHEN** an impl declares a type parameter that neither its capability nor target type uses
- **THEN** analysis reports a deterministic diagnostic naming the unused parameter and publishes no dispatchable conformance fact

#### Scenario: Reject Drop on always-Copy instantiations at monomorphization

- **WHEN** a parametric Drop conformance is instantiated at an element type making the whole provider Copy
- **THEN** the existing Copy-cannot-implement-Drop rejection fires for that instantiation with the instantiated type named in the diagnostic
