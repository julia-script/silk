## Context

See `proposal.md` for motivation. `ProjectAnalysis.revise` currently supplies the prior committed
closure only to `ModuleClosure`, so exact unchanged `SyntaxFile` values can be reused. `Pipeline`
then rebuilds a closure-wide `DeclarationIndex`, `NameResolution`, every `Elaboration.Result`, and
every `Ownership` result. `Elaboration.Result` retains the complete project index, so no current
semantic object can yet be safely replaced with its predecessor.

The completed declaration index already contains the semantic header facts that cross-module
resolution and elaboration observe. `ModuleClosure.Module.imports` supplies canonical dependency
outcomes, and the closure already detects import cycles. These are sufficient to build an
invalidation oracle after the current global frontend completes, without changing semantic phase
behavior or weakening immutable project commits.

## Goals / Non-Goals

**Goals:**

- Give module semantic meaning a small, compiler-owned, deterministically comparable interface.
- Compute the exact conservative set of modules that a future reuse implementation must recompute.
- Model import cycles and graph changes without introducing a topological ordering requirement.
- Make classifications, reasons, and counts testable independently of wall-clock thresholds.

**Non-Goals:**

- Reusing declaration, resolution, elaboration, ownership, diagnostic, or tooling objects.
- Removing `Elaboration.Result.index` or otherwise refactoring semantic artifact ownership.
- Incremental declaration collection, parsing, green/red syntax trees, or persistent caches.
- Making batch compiler results depend on analysis history.

## Decisions

### D1: Add a `ModuleSurface` actor over canonical semantic values

`ModuleSurface` owns one immutable value per module and `equals(left, right)`. Its value is a
canonical, ordered representation of all header meaning another module can distinguish:

- canonical module/member identity, declaration kind, name state, and visibility;
- function kind, type parameters, ordered parameter and result contracts, failure and requirement
  rows;
- nominal type parameters and ordered field contracts;
- constant type and value state;
- conformances, witnesses, and explicit unavailable/duplicate states that affect lookup or typing.

The representation uses canonical identities and semantic type keys, with type-aware length
framing for variable data. It deliberately omits syntax nodes, spans, diagnostic identities,
project object references, and bodies. Equality compares the complete canonical representation. A
hash may be added later as an index, but it cannot define equality.

Serializing arbitrary fact objects was rejected because source provenance, insertion-ordered maps,
and object graph shape would make body-only or position-only edits look semantic. Exposing every
header fact directly was rejected because callers would need to understand the entire declaration
model merely to compare meaning; the actor earns depth by centralizing that knowledge.

### D2: Compute surfaces after the complete declaration index

`Pipeline` adds one measured `module-surface` phase after declaration completion and name
resolution have produced final header facts. `FrontendFacts` retains a canonical module-keyed
surface map. Both single-root and project frontends therefore expose the same current semantic
meaning, while history-dependent comparison remains exclusively in `ProjectAnalysis.revise`.

Deriving surfaces directly from syntax was rejected because aliases, visibility, resolved nominal
identities, unavailable states, and compiler-known semantics are not syntax properties. Building
them in the LSP was rejected because semantic invalidation belongs to the compiler.

### D3: Add a `SemanticInvalidation` actor that plans over completed revisions

`SemanticInvalidation.between(previous, current, syntaxRevisions)` returns one immutable plan. The
actor internally constructs for each revision:

- an ordered local input: module identity, syntax reuse/change evidence, canonical resolved-import
  outcomes, and a compiler semantic-environment key;
- ordered dependency edges and dependency surfaces;
- revision components and reverse dependent edges.

The public plan remains small: canonical module observations plus deterministic totals. Each
observation is `Reusable` or `Recomputed` with ordered reasons such as `Fresh`, `LocalChange`,
`DependencySurfaceChange`, and `CyclicPeerChange`. Callers do not control individual phase caches
or supply hand-built dependency keys.

Putting invalidation flags on each compiler phase was rejected as a shallow interface that would
spread consistency rules across callers. Comparing only syntax identity was rejected as unsound
for changed imports and dependency contracts.

### D4: Propagate surfaces through SCCs with a worklist

The planner forms strongly connected components over the union of relevant previous and current
edges among current modules. This conservatively keeps former cycle peers together when an edit
splits a cycle and current peers together when an edit creates one. Component and member order is
canonical.

Initial work contains components with a fresh module, changed local syntax/origin, changed resolved
import outcome, or changed semantic-environment key. Every member of such a component is marked for
recomputation. After a component is considered, its aggregate current surface is compared with its
previous aggregate surface. Only an unequal aggregate surface enqueues current dependent
components. Thus a body-only edit recomputes its owner but does not reach importers; a public
contract edit reaches importers; and propagation stops when an intermediate surface stabilizes.

Naive topological analysis was rejected because Silk permits declaration cycles. Invalidating all
transitive dependents of any edited module was rejected because it loses the body-only and
surface-stabilization wins the design exists to identify.

### D5: Publish future-work observations without changing current work

`ProjectAnalysis.make` creates a plan that classifies all current modules as fresh.
`ProjectAnalysis.revise` computes syntax observations, then semantic invalidation against the prior
completed project. It appends one semantic-invalidation `PhaseReport` whose inputs are total
modules, outputs are planned recomputations, and deterministic counters include reusable,
recomputed, and reason totals. The plan contains the exact per-module evidence behind the counts.

Root views and the project reference the same surface map, plan, and final report. Nevertheless,
`Pipeline` continues to run every semantic and tooling phase globally and every semantic table in
the new project is freshly constructed. Tests assert both the expected future plan and the absence
of premature semantic object sharing.

Treating `PhaseReport.outputs` alone as the whole interface was rejected because it cannot explain
why individual modules invalidated. Adding timing thresholds was rejected because correctness and
performance evidence have different stability requirements.

### D6: Preserve the committed-predecessor and atomic-snapshot model

No LSP cache is added. `Workspace` continues to retain the shared compiler `ProjectAnalysis`, and
`ProjectSession` continues to provide only the last atomically committed analyzed-document map to
the next callback. A stale computation may construct surfaces and a plan but cannot commit and
therefore cannot seed later invalidation.

A hidden process-global previous project was rejected because it would mix projects, escape
session lifetime, and make accepted-revision semantics implicit.

## Risks / Trade-offs

- **[Risk] Surface omission could create a false reusable classification.** → Encode every
  cross-module-observable completed header state and add mutation tests for every header family;
  future reuse remains disabled until the oracle is established.
- **[Risk] Overly detailed surfaces could propagate harmless edits.** → Exclude bodies, spans,
  diagnostics' source identities, and object identity; prefer conservative false invalidation over
  false reuse for unresolved cases.
- **[Risk] Graph merges, splits, or missing imports could evade current-only SCCs.** → Build
  revision components from previous/current edge evidence and treat resolved-import outcome changes
  as semantic inputs.
- **[Trade-off] This change adds surface encoding and planning after a still-global frontend.** →
  Keep the work linear in module/header/edge counts, measure it through phase reports, and use it to
  unlock actual reuse in the next change.
- **[Risk] Optional phase counters could become an unstructured metrics bag.** → Define stable
  semantic-invalidation counter names in the actor and keep per-module evidence in the typed plan.

## Migration Plan

1. Introduce and test canonical module surfaces against every currently observable header family.
2. Introduce SCC-aware invalidation planning and deterministic reason/count fixtures.
3. Integrate surfaces into `Pipeline` and plans into `ProjectAnalysis`, then extend root-view and LSP
   stale-revision tests.
4. Run the complete repository and package-export gates, sync the capability specs, and archive.

Rollback removes the new observations and surface phase. No persisted cache, protocol shape, or
mutable state requires migration.
