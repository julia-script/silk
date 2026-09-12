## ADDED Requirements

### Requirement: Effect environments admit finite lifetime intersections

Effect environment annotations SHALL accept a finite intersection of named lifetimes using
`'a & 'b`, including explicit effect-function environment annotations. An intersection SHALL
express common validity and SHALL NOT equate or extend any constituent lifetime. Intersections
SHALL be associative, commutative, and idempotent, with static validity as identity. Their
semantic identity SHALL be independent of constituent spelling, order, source offsets, and
ambient proof assumptions. Substitution and runtime erasure SHALL preserve the existing
lifetime contract for each constituent.

#### Scenario: Retain independent captured and invocation sources

- **WHEN** a callback's Effect environment is `'scope & 'env`
- **THEN** running the Effect requires both the invocation source and captured environment to remain valid, without proving either source outlives the other

#### Scenario: Reject promotion from an intersection

- **WHEN** an Effect valid for `'short & 'long` is supplied where validity for all of `'long` is required without a sufficient declared outlives proof
- **THEN** the compiler rejects the lifetime mismatch

#### Scenario: Preserve canonical meaning

- **WHEN** an environment intersection is reordered, repeated, or intersected with `'static`
- **THEN** equivalent normalized expressions have the same semantic identity and no runtime lifetime argument or layout lane is added

#### Scenario: Derive a shorter usable region

- **WHEN** each constituent of an environment intersection is proven to outlive a run region
- **THEN** the intersection can satisfy that run region while concrete loan and ownership obligations remain required

## MODIFIED Requirements

### Requirement: Effect environment and outcome validity remain independent

`Effect<'env; A ! E ? R>` and `effect<'env> fn` SHALL preserve environment validity separately from complete success/failure types, required services, exact representation and execution access. External outcome sources SHALL survive producing temporaries, handlers, retry and cancellation. Run-local or consumed environment-owned referents SHALL NOT escape.

An intersection environment SHALL retain every constituent validity dependency without changing
success, failure, requirement, representation, or access channels. An inferred common lower-bound
lifetime SHALL NOT be promoted to the entire intersection unless its complete retention contract
establishes that relationship. Generic retained contents SHALL continue to require their own
validity obligations.

#### Scenario: Release a temporary receiver

- **WHEN** an Effect captures `&'call mut Holder<'data>` and returns an external `&'data A`
- **THEN** ending the temporary Effect releases the receiver borrow while the result retains the external source obligation

#### Scenario: Reject consumed scratch

- **WHEN** a consuming Effect returns a reference into its own captured or run-local storage
- **THEN** the compiler rejects the escaping outcome before lowering

#### Scenario: Keep generic retained contents checked

- **WHEN** an anonymous Effect retains generic captured state and a resource borrow
- **THEN** the intersection environment is accepted only with the complete captured-content and resource well-formedness obligations, without assuming the generic state is detached
