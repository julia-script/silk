## ADDED Requirements

### Requirement: Parametric conformances join the canonical index

The declaration index SHALL record parametric conformances with their type-parameter lists as
canonical header-level facts, validating restricted Drop hook shape against the generic target type
with the impl's parameters in scope. Validation that depends on a concrete instantiation — such as
the Copy prohibition — SHALL be deferred to instantiation rather than rejected at the header.

#### Scenario: Index a parametric Drop hook

- **WHEN** the index processes `impl<T> Drop for Vector<T>` whose hook is `fn drop(self: &mut Vector<T>) -> Unit`
- **THEN** the hook validates with `T` in scope and the conformance fact records the parameter list and generic target

#### Scenario: Header validation still rejects malformed hooks

- **WHEN** a parametric Drop hook declares extra parameters, a failure row, or a mismatched self type
- **THEN** the index reports the existing invalid-Drop-hook diagnostic without waiting for an instantiation
