## MODIFIED Requirements

### Requirement: Parametric conformances join the canonical index

The declaration index SHALL record parametric conformances with their complete kinded parameter
lists, conditional interface requirements, canonical provider/interface heads, mapped operations,
visibility, overlap state, and structural-termination facts. It SHALL validate restricted Drop hook
shape against the generic target and SHALL conservatively reject possibly overlapping conditional
heads without consulting their bounds. Validation that depends on a concrete instantiation SHALL be
deferred to specialization.

#### Scenario: Index a parametric Drop hook
- **WHEN** the index processes `impl<T> Drop for Vector<T>` whose hook is `fn drop(self: &mut Vector<T>) -> ()`
- **THEN** the hook validates with `T` in scope and the conformance fact records the parameter list and generic target

#### Scenario: Index a conditional user conformance
- **WHEN** a wrapper conformance declares one strict-subterm provider requirement
- **THEN** the canonical header records that requirement and its finite structural measure

#### Scenario: Reject overlapping conditional heads
- **WHEN** two declarations have provider/interface heads that may unify despite different bounds
- **THEN** indexing reports deterministic overlap before either bound is proved

#### Scenario: Header validation still rejects malformed hooks
- **WHEN** a parametric Drop hook declares extra parameters, a failure row, or a mismatched self type
- **THEN** the index reports the existing invalid-Drop-hook diagnostic without waiting for an instantiation
