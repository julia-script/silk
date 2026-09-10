## MODIFIED Requirements

### Requirement: Semantic facts expose generic binding and specialization

Semantic analysis SHALL publish canonical facts for type- and lifetime-parameter declarations and references,
applied nominal types, inferred and explicit call arguments, substitutions, and unavailable
specializations. Every fact SHALL retain source provenance and causal diagnostic identity.

#### Scenario: Inspect an inferred substitution

- **WHEN** a generic call infers `T` as `Token` from its argument
- **THEN** semantic facts expose the parameter, concrete argument, inference source, and specialized result type

#### Scenario: Preserve a conflicting inference

- **WHEN** two arguments require incompatible types for one parameter
- **THEN** facts retain both constraints and one deterministic specialization diagnostic

### Requirement: Slice types and borrows retain canonical semantic facts

Semantic analysis SHALL publish slice type facts containing canonical lifetime, element type, and shared or
exclusive access without a fixed length. Each explicit borrow or reborrow SHALL retain its access,
stable source root, source type, resulting slice type, call destination, exact syntax provenance,
and an explicit unavailable state when any prerequisite is missing. Every borrow SHALL retain its
stable logical owner and complete field or checked-index selector path. An owned temporary SHALL
receive a deterministic compiler-owned identity rather than requiring a source binding name.

#### Scenario: Resolve different arrays to one slice type

- **WHEN** `&short` and `&long` borrow `Array<i32, 3>` and `Array<i32, 6>` for `&[i32]`
- **THEN** both borrow facts retain their distinct source types and semantic lifetimes as appropriate while sharing the same lifetime-erased element/access runtime shape

#### Scenario: Preserve an invalid exclusive borrow

- **WHEN** `&mut values` targets an immutable array binding
- **THEN** the fact retains exclusive intent, the resolved source root and type, and the diagnostic cause without claiming an available exclusive slice

#### Scenario: Inspect temporary and indexed roots

- **WHEN** one function borrows `&[1, 2]` and another borrows `&matrix[index]`
- **THEN** facts distinguish a hidden temporary owner from a named root plus runtime index selector

### Requirement: Semantic facts derive detached and non-parking executable properties

Analysis SHALL derive `Intrinsic.Detached` for an exact value or executable representation only
when an ordinary data value's complete semantic type has no non-static borrowed-content requirement, or an executable's complete environment type has no such requirement and its exact representation or explicit detached representation bound establishes independence from external lexical or provider loans required for invocation and cleanup. Executable environment detachment SHALL NOT establish detached success or failure outcomes; completion and outcome admission SHALL check those boundaries independently. Static shared views SHALL remain eligible; lifetime-bearing nested fields, string views, and environment bounds SHALL be included. An empty Effect requirement row MUST NOT by itself establish Detached. Analysis
SHALL derive `Intrinsic.NonParking` for an exact callable only when its specialized transitive call
graph cannot reach `Intrinsic.park`; direct work and nested-only suspension SHALL remain permitted.
Open generics SHALL preserve either fact only through an explicit sealed-property bound. Facts and
failure causes SHALL be deterministic under substitution, caching, serialization, and inspection,
and ordinary source declarations MUST NOT acquire either property by spelling. A failed Detached or
NonParking bound/application SHALL publish one deterministic source diagnostic at the failed
obligation, retain the complete lexical/provider-loan or transitive-park causal path, and remain
distinct from an unsatisfied Effect requirement-row diagnostic.

#### Scenario: Reject a captured lexical loan

- **WHEN** an exact Effect captures a caller-owned shared or exclusive reference
- **THEN** its Detached fact is unsatisfied with the capture provenance as the cause

#### Scenario: Distinguish an owned provider from a borrowed provider

- **WHEN** one closed Effect retains a provider loan and another owns an equivalent detached provider value
- **THEN** the first fails Detached despite its empty requirement row and the second satisfies Detached when every nested capture is detached

#### Scenario: Keep detachment orthogonal to local execution affinity

- **WHEN** a closed Effect owns a local `Shared<T>` handle and retains no external lexical or provider loan
- **THEN** it satisfies Detached while its canonical execution affinity remains `LocalExecution`, and neither fact grants thread transfer

#### Scenario: Inspect a nested nominal loan

- **WHEN** an executable owns a nominal whose nested field retains an external loan
- **THEN** Detached is unsatisfied and inspection preserves the complete field-to-loan cause

#### Scenario: Keep payload opacity independent

- **WHEN** an exact executable has an opaque producer result type but its complete environment is owned
- **THEN** Detached remains satisfied because the proof concerns environment and drop provenance rather than success or failure payload spelling

#### Scenario: Admit nested-only work in a non-parking callback

- **WHEN** an exact callback performs direct work and can reach `Effect.suspend` but cannot reach `Intrinsic.park`
- **THEN** NonParking is satisfied

#### Scenario: Reject transitive external parking

- **WHEN** an exact callback reaches `Intrinsic.park` through a specialized provider or ordinary helper
- **THEN** NonParking is unsatisfied and reports its stable diagnostic at the failed obligation with a deterministic transitive-reachability cause

#### Scenario: Diagnose detached admission independently of requirement rows

- **WHEN** a closed Effect has an empty requirement row but retains an external provider loan at an Execution wrapper call
- **THEN** analysis reports the Detached diagnostic at the failed bound/application with the provider path and does not report an unsatisfied requirement row

#### Scenario: Ignore privileged-looking source names

- **WHEN** ordinary source declares types or functions named Execution, Wake, Detached, NonParking, Scheduler, Fiber, Deferred, Timer, or Coroutine
- **THEN** semantic facts grant no intrinsic identity, property, or suspension mode by spelling

#### Scenario: Reject a constrained empty variant

- **WHEN** an empty union variant is already constrained to a short borrowed payload lifetime
- **THEN** Detached remains unsatisfied despite the active payload being empty; a freshly constructed unconstrained empty value is a distinct case

## ADDED Requirements

### Requirement: Lifetime and initialization facts are canonical and inspectable

Semantic inspection SHALL publish authored and inferred declaration-relative binders, substitutions, outlives assumptions, well-formedness and variance summaries, environment bounds, and explicit unavailable outcomes with diagnostic identity. Ownership inspection SHALL separately expose source places, loan ancestry, retaining paths, move-path initialization and reachability, restoration, and conditional cleanup. Diagnostics SHALL connect the borrow source, retained value, conflicting invalidation, required later use or cleanup, and applicable partial-move boundary. Presentation SHALL use stable readable names without exposing local compiler IDs as public contracts.

#### Scenario: Inspect a failed holder reset

- **WHEN** a replacement source expires before the holder's required later use or cleanup
- **THEN** facts retain the invariant destination lifetime, source borrow, attempted installation, and later requirement as diagnostic witnesses

#### Scenario: Inspect a rejected partial owner use

- **WHEN** a whole-value borrow follows a conditional field move
- **THEN** facts identify the moved path and branch join, initialized siblings, and the complete-value operation that failed
