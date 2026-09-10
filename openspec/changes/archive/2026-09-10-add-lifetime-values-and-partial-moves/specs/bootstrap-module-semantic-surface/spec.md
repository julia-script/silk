## MODIFIED Requirements

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

#### Scenario: Revalidate only an edited semantic body

- **WHEN** one private body changes among unrelated declarations while all directly consumed exported facts stay equal
- **THEN** the edited body is checked again but sibling and downstream semantic body query executions remain reusable; dependency-fingerprint validation is distinguished from executing a body checker

#### Scenario: Invalidate an exported lifetime consumer

- **WHEN** an exported outlives relationship, variance summary, environment bound, or consumed cleanup fact changes
- **THEN** only actual semantic consumers invalidate, including a negative invalidation witness that rules out an always-reuse cache

### Requirement: Cyclic dependencies invalidate as conservative components

Import-graph planning SHALL identify import components for deterministic dependency validation, while semantic body recomputation SHALL follow actual consumed declaration edges and actual recursive semantic components. A changed member SHALL NOT force unrelated bodies in the same module or merely cyclic import group to rerun. Dependency merges and splits SHALL validate sufficient previous and current graph facts to avoid stale reuse. Propagation beyond a changed component SHALL occur only when consumed semantic facts change.

#### Scenario: Edit an actual recursive semantic component

- **WHEN** `A` and `B` form an actual recursive semantic declaration component and either member changes an input consumed throughout that component
- **THEN** the plan marks the affected component members for recomputation without broadening that execution to unrelated declarations

#### Scenario: Keep a dependent outside a stable cycle reusable

- **WHEN** `C` depends on cyclic modules `A` and `B` and the cycle is recomputed without changing its exposed surfaces
- **THEN** the plan keeps `C` reusable

#### Scenario: Edit one member of a cycle

- **WHEN** A and B import each other and one body changes without changing any exported fact consumed by the other
- **THEN** the import component is dependency-validated but only affected semantic bodies recompute

## ADDED Requirements

### Requirement: Module surfaces encode declaration-relative lifetime contracts

Module surfaces SHALL retain canonical binder structure, reference and nominal lifetime arguments, outlives and implied well-formedness relationships, derived variance, quantified callable contracts, environment bounds, and consumed representation or cleanup facts. Surface equality SHALL ignore alpha-renaming while preserving semantic changes. Concrete loan IDs, caller roots, local inference IDs, and partial initialization subsets SHALL remain absent. Explicit static body and code-generation dependencies SHALL retain separate implementation fingerprints and MUST NOT masquerade as semantic interface changes.

#### Scenario: Publish an inferred field contract

- **WHEN** a public holder declares two omitted field lifetimes
- **THEN** its surface exposes two canonical independent binders and clients consume the same relationships as an explicit declaration

#### Scenario: Keep body-dependent static work honest

- **WHEN** a consumer explicitly evaluates an exported body in its upstream static context
- **THEN** a body edit invalidates that recorded dependency even when ordinary signature-only consumers remain reusable
