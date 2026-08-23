## Context

Current Silk already computes suspendability, exact executable identities, capture provenance,
continuation liveness, and nested suspension frames. See `proposal.md` and the four delta specs. The
design must extend those facts without coupling semantic analysis to a source Scheduler or choosing
storage before the packaging slice.

## Goals / Non-Goals

**Goals:**

- Publish one normalized static suspension summary and two sealed executable properties.
- Make explicit construction the only propagation delimiter for external parking in this slice.
- Represent the lifecycle and ownership facts without prescribing runtime tags or backend layouts.

**Non-Goals:**

- Add construction, drive, wake, park, implicit entry ownership, or canonical concurrency actors.
- Define thread transfer, atomic ordering, or a public continuation representation.

## Decisions

### Normalize one three-mode reachability summary

Extend the existing suspendability fact into a canonical bitset/closed outcome for nested transfer
and external parking, with direct execution represented by neither bit. Complete specializations
normalize from their selected call/provider graph; open applications union the modes permitted by
their contracts. Construction records a delimiter edge so external-park reachability does not flow
back through the owner-side `drive` call.

Alternative rejected: a single may-suspend bit cannot preserve the cheaper nested path or prove
NonParking; actor-name inference violates the intrinsic boundary.

### Derive Detached from the complete environment-dependency graph

Reuse canonical capture, nominal-field, provider-binding, and borrow-root provenance. Detached is a
closed proof over later invoke and drop dependencies, not a property of `A`, the error row, or the
requirement row alone. Preserve an ordered causal path for every failing dependency so generic and
concrete diagnostics agree.

Alternative rejected: a specialization-only hidden check makes safe generic wrappers impossible to
state and hides provider-loan failures.

### Derive NonParking from specialized transitive reachability

NonParking consults only the external-park bit. Nested suspension remains legal, which keeps the
property narrow enough for runtime-invoked callbacks without creating general effect-polymorphic
callable types.

Alternative rejected: requiring callbacks to be wholly non-suspending would reject valid nested
work and broaden the feature's type surface.

### Preserve exact representation identity under sealed conjuncts

Represent sealed properties as ordered obligations attached to one exact executable binder. They
participate in substitution and cache keys but emit no runtime dictionary. Reject ordinary
interface/service conjuncts in this syntax lane.

Alternative rejected: an interface model would erase exact representation identity and introduce
nominal conformance or runtime evidence that the execution initializer does not need.

### Publish abstract lifecycle states separately from storage state

Semantic/ownership artifacts use Initial, Running, Dormant, Notifying, Eligible, Completed, and
Destroyed. Wake-cell coordination states belong to slice 3. This separation allows backends to fuse
physical tags while keeping illegal drive states and owner-visible transfers auditable.

Alternative rejected: a Scheduler-shaped Ready/Parked task state would make source policy part of
the intrinsic lifecycle.

## Risks / Trade-offs

- **Reachability summaries become stale after specialization** → include selected providers and
  exact executable identities in canonical dependency/cache keys and re-normalize at realization.
- **Detached causes become noisy** → preserve one structured root-to-loan path per distinct cause
  and let diagnostics present the shortest stable path.
- **Lifecycle names are mistaken for a runtime ABI** → keep them in target-neutral facts and tests;
  backend tags and layouts remain private.
- **The exact-bound rule grows into intersections** → parser/semantic tests admit only one exact
  executable bound plus the closed sealed-property set.
