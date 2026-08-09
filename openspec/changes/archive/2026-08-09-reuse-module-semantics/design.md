## Context

The semantic invalidation oracle now runs after a globally constructed declaration index and
module surfaces, but `Pipeline.frontendProject` has already elaborated and ownership-checked every
module by then. `Elaboration.Result` also retains the complete `DeclarationIndex.Index`, preventing
an otherwise reusable module result from having an honest module-owned lifetime. Project root views
are typed as ordinary `Analysis.FrontendSnapshot` values even though their semantic universe is the
multi-root union. See `proposal.md` and the three delta specs for the required behavior.

## Goals / Non-Goals

**Goals:**

- Reuse elaboration/HIR and ownership together at module granularity under the existing oracle.
- Make module semantic artifacts independent of a predecessor project-index object.
- Preserve exact current source navigation, immutable project snapshots, SCC invalidation, and
  atomic LSP commits.
- Make reuse visible through deterministic phase counters and object-identity tests.
- Prevent project root views from entering runtime realization through the public type system.

**Non-Goals:**

- Incremental declaration collection/completion, name resolution, surface construction, merged
  diagnostics, or tooling-index composition.
- Mutable caches, reference patching, green/red syntax trees, runtime realization of project views,
  or LSP-owned semantic state.
- Resolving the separate Copy-vocabulary authority or decomposing the large elaboration actor.

## Decisions

### Introduce one module-owned semantic artifact

Add a `ModuleSemantics` actor whose immutable value owns the canonical module name, one
`Elaboration.Result`, and one `Ownership.ModuleOwnership`. Remove `index` from
`Elaboration.Result`; `Ownership.checkModule` receives the current index explicitly while building
a new artifact. This makes the artifact boundary honest without adapting or mutating reused values.

The elaboration graph may still contain immutable declaration facts captured while resolving a
dependency. Those facts are semantic inputs, not membership handles into a project index. Reuse is
permitted only when every dependency surface is equal. Source-backed presentation never trusts a
captured dependency object's syntax: it resolves canonical identities against the current index.

Alternatives rejected:

- Retaining `Result.index` would share the complete predecessor closure and make current-project
  ownership and query behavior ambiguous.
- Rebinding or patching old declaration objects would add translation cost, identity hazards, and
  casts precisely where immutable structural sharing should be simplest.
- Replacing every semantic fact reference with a serialized identity in this pass would be a much
  larger fact-model migration than required for safe module reuse.

### Move invalidation before project elaboration

`Pipeline.frontendProject` accepts an optional prior project reuse basis. It still loads the current
closure and globally computes collected/completed declarations, name resolution, and module
surfaces. It then builds the existing invalidation plan before elaboration. For each current module:

1. If the plan says `Reusable` and the prior basis contains the same module artifact, retain that
   artifact by reference.
2. Otherwise elaborate using current syntax, current module headers, current scope, and current
   index, then ownership-check it with the current index and create a new artifact.

Fresh analysis supplies no prior basis, so every module recomputes. The result maps exposed by the
existing facade are derived from the artifact map, keeping existing query structure while making
ownership explicit. Tooling still runs once over the completed mixed project.

Alternatives rejected:

- Computing invalidation in `ProjectAnalysis` after `frontendProject` cannot avoid semantic work.
- Using syntax identity alone is unsound when dependency meaning changes.
- Reusing only acyclic modules is unnecessary because the oracle already invalidates full affected
  SCCs conservatively.

### Keep declaration and resolution phases global in this change

The current surface oracle depends on completed headers and resolved dependency outcomes. Those
phases remain history-independent and globally reconstructed, producing the current authority for
queries and presentation. This intentionally captures a narrower, measurable win and avoids
inventing a second incremental validity model before module semantic reuse is proven.

The next proposal makes `SemanticOccurrence` and anonymous-expression indexes composable. Header
and name-resolution incrementality can be measured and proposed separately afterward if they remain
dominant.

### Canonicalize source presentation through current facts

Semantic occurrence construction resolves any canonical declaration referenced by an elaboration
fact through the current declaration index before deriving definition spans or field locations.
Local declarations in a reused module already retain exact syntax because local syntax equality is
required for reuse. This one-way lookup is normal current-project query construction, not mutation
or reference patching.

### Report reuse on semantic work phases

Extend `PhaseReport` with a typed module-reuse counter carrying `reused` and `recomputed`. The
project invalidation phase retains its reason counters. Elaboration and ownership reports count
only newly executed module work while also publishing total reuse/recomputation counters, so tests
can prove actual execution without timing thresholds.

### Give project views an explicit non-realizable discriminator

Keep the shared frontend-query shape but add a required realization-kind discriminator. A
single-root `Analysis` snapshot carries `SingleRoot`; `ProjectAnalysis.View` carries `ProjectView`.
`Analysis.realize` accepts only the narrowed single-root type. LSP document/workspace APIs use the
query-compatible project view type where appropriate. This is a breaking type correction with no
runtime behavior change.

## Risks / Trade-offs

- [Captured dependency facts may carry predecessor syntax] → Only semantically equal dependencies
  permit reuse; all source locations are canonicalized through the current index, with a moved-span
  regression test.
- [Global header and resolution work limits the speedup] → Phase reports expose the remaining cost;
  this change targets the much larger elaboration/ownership actors without overextending validity.
- [A missing prior artifact could disagree with the oracle] → Missing or module-mismatched artifacts
  always force recomputation and the actual reuse counters reflect that conservative fallback.
- [Mixed reused/new maps could become incoherent] → Build the complete artifact map before tooling,
  diagnostics, views, or project publication; no consumer sees a partial project.
- [Public type correction causes call-site churn] → Update compiler and LSP callers atomically;
  pre-release policy permits the break and no compatibility shim is retained.

## Migration Plan

1. Add the module artifact and realization-kind types, then remove the result/index closure edge.
2. Move project invalidation into the project pipeline and select reused versus recomputed module
   artifacts before tooling construction.
3. Canonicalize source locations, add deterministic counters, and update compiler/LSP callers.
4. Verify focused reuse, stale-revision, navigation, facade typing, and determinism tests before the
   repository-wide checks.

Rollback is one feature-branch revert because artifacts remain immutable and no persisted cache or
external data migration is introduced.
