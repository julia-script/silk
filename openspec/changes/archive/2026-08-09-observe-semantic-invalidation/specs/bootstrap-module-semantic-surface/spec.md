## Purpose

Defines deterministic module-level semantic surfaces and dependency-aware invalidation evidence that
can safely govern reuse of immutable compiler artifacts across adjacent project revisions.

## ADDED Requirements

### Requirement: Every analyzed module publishes a deterministic semantic surface

The compiler SHALL publish one immutable semantic surface per current module. The surface SHALL
encode every module-header fact that another module's resolution or type analysis can observe,
including member identity, declaration kind, visibility, callable contracts, nominal struct
contracts, constants, conformances, and explicit unavailable states. It SHALL exclude function
bodies, source spans, syntax-object identity, project-index identity, and iteration order.

#### Scenario: Repeat a surface in fresh processes

- **WHEN** equivalent module headers are analyzed in separate fresh processes
- **THEN** their ordered semantic surfaces and canonical encodings are equal

#### Scenario: Change only a function body

- **WHEN** a module changes a function body without changing any observable header fact
- **THEN** its new semantic surface equals its previous semantic surface

#### Scenario: Preserve an unavailable header state

- **WHEN** parser recovery or semantic analysis leaves a public header fact unavailable
- **THEN** the surface retains the deterministic unavailable state rather than substituting the previous valid contract

### Requirement: Semantic-surface equality is exact and independent of hashing

Surface equality SHALL compare the complete canonical semantic representation. An implementation
MAY derive a digest for lookup or reporting, but a digest match MUST NOT establish equality without
the exact semantic representation, and process-local object identity MUST NOT affect equality.

#### Scenario: Compare independently allocated equal facts

- **WHEN** two independently allocated module-header graphs have the same semantic meaning
- **THEN** their surfaces compare equal despite having no shared object references

#### Scenario: Compare different public contracts

- **WHEN** two surfaces differ by parameter contract, visibility, nominal field contract, constant value, conformance, or unavailable state
- **THEN** they compare unequal even if a candidate digest collides

### Requirement: Invalidation is dependency- and surface-aware

For adjacent completed project revisions, the compiler SHALL derive a deterministic invalidation
plan from local semantic inputs, resolved dependency inputs, dependency semantic surfaces, and the
compiler semantic environment. A fresh or locally changed module SHALL be marked for recomputation.
A dependent module SHALL be marked only when a direct dependency's exposed surface or resolved
dependency input changed. Recomputing a dependency whose surface remains equal SHALL NOT by itself
invalidate its dependents.

#### Scenario: Edit an unrelated module

- **WHEN** module `A` depends on `B` and unrelated module `C` changes
- **THEN** the plan marks `C` for recomputation and keeps `A` and `B` reusable

#### Scenario: Edit only a dependency body

- **WHEN** `B` calls a public function from `A` and only the body of that function changes
- **THEN** the plan marks `A` for local recomputation and keeps `B` reusable because `A`'s surface is equal

#### Scenario: Change a dependency signature

- **WHEN** `A.answer() -> i32` changes to `A.answer(i32) -> i32` while importer `B` is unchanged
- **THEN** the plan marks `A` for local recomputation and `B` for dependency-surface recomputation

#### Scenario: Change a dependency visibility

- **WHEN** an imported declaration changes from public to private
- **THEN** its unchanged importers are marked for dependency-surface recomputation

#### Scenario: Change a nominal struct shape

- **WHEN** a public struct gains or loses a field or changes a field contract
- **THEN** modules whose semantic inputs include that struct surface are marked for dependency-surface recomputation

### Requirement: Surface propagation stops when exported meaning stabilizes

Invalidation SHALL propagate through dependents according to the newly analyzed semantic surface,
not merely because an intermediate module was recomputed. If an invalidated module's resulting
surface equals its previous surface, its dependents SHALL remain reusable unless another semantic
input changed.

#### Scenario: Body-only change through a dependency chain

- **WHEN** `A` depends on `B`, `B` depends on `C`, and only a body in `C` changes
- **THEN** the plan recomputes `C` and keeps `B` and `A` reusable when `C`'s surface remains equal

#### Scenario: Surface change stabilizes at an intermediate module

- **WHEN** `C` changes its public surface, `B` must be recomputed, and `B`'s resulting surface remains equal
- **THEN** the plan marks `C` and `B` for recomputation but keeps dependent `A` reusable

### Requirement: Cyclic dependencies invalidate as conservative components

Modules participating in an import cycle SHALL be planned as one deterministic strongly connected
component. A local or dependency-surface change affecting any member SHALL mark every current
member of that component for recomputation. Dependency-graph changes SHALL be evaluated over enough
previous and current graph information to handle component merges and splits conservatively.
Propagation beyond the component SHALL occur only when its exposed surfaces change.

#### Scenario: Edit one member of a cycle

- **WHEN** `A` and `B` import one another and either member changes a local semantic input
- **THEN** the plan marks both `A` and `B` for recomputation

#### Scenario: Keep a dependent outside a stable cycle reusable

- **WHEN** `C` depends on cyclic modules `A` and `B` and the cycle is recomputed without changing its exposed surfaces
- **THEN** the plan keeps `C` reusable

### Requirement: Invalidation evidence is complete and deterministic

The plan SHALL contain exactly one observation for every current module, classifying it as reusable
or recomputed with an ordered non-empty set of reasons. It SHALL expose deterministic totals for
modules, reusable modules, recomputed modules, and each reason. Removed prior modules SHALL not
appear as current observations.

#### Scenario: Observe a mixed revision

- **WHEN** a revision has one fresh module, one local edit, two dependency-surface invalidations, and three reusable modules
- **THEN** the observations and totals report those exact classifications in canonical module order

#### Scenario: Repeat planning with another root order

- **WHEN** equivalent adjacent projects are supplied with roots or maps in another iteration order
- **THEN** the invalidation observations, reasons, and totals are identical
