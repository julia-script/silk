## Purpose

Define stable exact and opaque source contracts for functions that return representation-dependent
values while keeping every realization static, private, and monomorphic.

## ADDED Requirements

### Requirement: Exact identities name resolved visible callable items

`typeof(item)` SHALL name the exact representation of one fully specialized named callable item.
The item MUST be at least as visible as the exposing contract; local bindings, sections, Effect
construction sites, private leaks, overloaded items, and partially specialized generic items MUST
be rejected with a diagnostic directing source to an opaque result when appropriate.

#### Scenario: Name a specialized public function
- **WHEN** a public result contains `typeof(identity<i32>)` and that item is sufficiently visible
- **THEN** the result contract records the exact named-item representation

#### Scenario: Reject a private exact identity leak
- **WHEN** a public result names a private function through `typeof`
- **THEN** analysis rejects the public contract and suggests an opaque representation result

### Requirement: Opaque results bind one static representation family

The contextual result syntax `some<F: Contract> Result` SHALL bind one declaration-owned opaque
representation over the complete result. Its stable family key SHALL use producer identity and
binder ordinal; its public signature SHALL also include the normalized bound, result occurrences,
and enclosing binder kinds.

#### Scenario: Return one capturing parser family
- **WHEN** repeated calls to one producer capture different runtime values at the same specialization
- **THEN** callers observe one opaque family instance and one layout while the captures remain data

#### Scenario: Keep producers distinct
- **WHEN** two declarations expose equivalent opaque bounds and result shapes
- **THEN** their family keys remain distinct and their results do not join implicitly

### Requirement: Opaque families specialize over enclosing arguments

An opaque family instance SHALL include every enclosing concrete type, row, and representation
argument that can affect its realization. All reachable returns for one producer specialization and
opaque binder MUST resolve to one concrete representation; divergent return identities MUST fail in
the producer.

#### Scenario: Specialize a generic opaque producer
- **WHEN** one opaque producer is called at `i32` and `Token`
- **THEN** each specialization receives its own family instance and may realize a different layout

#### Scenario: Reject divergent opaque branches
- **WHEN** one opaque producer branch returns a hex parser and another returns a decimal parser
- **THEN** the producer is rejected because one family instance cannot have two realizations

### Requirement: Private realizations drive invalidation

Each externally used opaque family SHALL publish a compiler-private realization definition with
target or runner, concrete arguments, capture layout, access, cleanup, and deterministic body and
layout fingerprints. Fingerprints SHALL drive dependent invalidation without participating in
source type equality or revealing private representation details.

#### Scenario: Preserve identity across a value-only edit
- **WHEN** a producer changes captured data without changing target or capture shape
- **THEN** its public opaque identity and reusable layout remain stable while the producer body invalidates

#### Scenario: Invalidate a capture-shape edit
- **WHEN** a hidden realization adds an owned capture
- **THEN** dependent ownership, layout, MIR, and emitted artifacts invalidate even though the public
  opaque signature is unchanged

### Requirement: Opaque realization is finite and non-existential

An opaque result SHALL NOT package a runtime type, choose a representation per execution, allocate
implicitly, or dispatch indirectly. A recursive producer MUST establish its realization from a local
construction before recursive use; realization-only cycles and inline layout cycles MUST be rejected.

#### Scenario: Reject a realization-only recursion
- **WHEN** an opaque producer's only representation evidence is a recursive call to itself
- **THEN** analysis reports an opaque-realization cycle before instance discovery
