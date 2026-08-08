## ADDED Requirements

### Requirement: Place replacement swaps one writable place atomically

The compiler SHALL provide `Place.replace(place, value)`: the first argument resolves as a
writable place under the same rules as assignment, the second as a value of the place's type, and
the expression yields the place's previous value. The place SHALL remain initialized throughout,
so an affine owner can leave a struct field behind an exclusive reference without a partial move.
Invalid places SHALL be rejected with the existing assignment diagnostics.

#### Scenario: Swap an affine union field through an exclusive reference

- **WHEN** a function swaps `self.state` for a replacement member through `&mut self` and consumes the returned old value
- **THEN** the program compiles without partial-move diagnostics and all three engines observe the old value exactly once and the replacement thereafter

#### Scenario: Reject an unwritable place

- **WHEN** the first argument is rooted in an immutable binding or a shared reference
- **THEN** the compiler reports the same deterministic diagnostic the equivalent assignment would receive

### Requirement: Slot copy reads Copy elements without consuming them

The unsafe storage vocabulary SHALL include `Slot.copy`: a qualified unsafe operation that reads
one initialized slot's value without ending its initialization, valid only for Copy element
types. A copy of a non-Copy element SHALL be rejected when the concrete instantiation is
verified, and initializedness remains the unsafe caller's obligation exactly as for `Slot.take`.

#### Scenario: Copy twice then take once

- **WHEN** unsafe code copies the same initialized Copy slot twice and then takes it
- **THEN** both copies observe the stored value, the take still succeeds, and all three engines agree on the result

#### Scenario: Reject a non-Copy element copy

- **WHEN** unsafe code copies a slot whose concrete element type owns cleanup obligations
- **THEN** the instantiation is rejected before any engine executes it

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
