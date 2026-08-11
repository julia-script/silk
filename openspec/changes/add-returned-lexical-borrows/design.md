## Context

Silk's current slice model deliberately limits borrows to call-scoped parameters. That is sound but
too shallow for ordinary source abstractions such as `Vector.asSlice`: the wrapper must return a
view of storage owned by its caller. General lifetime parameters and stored borrows would solve a
larger problem than these abstractions require and would substantially widen type identity, layout,
and effect design.

This change introduces one conservative bridge: an ordinary function result may carry a lexical
borrow from exactly one borrowed parameter. The compiler continues to reject owned storage of any
lifetime-bearing value.

## Goals / Non-Goals

**Goals:**

- support shared and exclusive returned slice views with one statically known origin;
- prevent source-owner mutation, movement, and cleanup while a returned view is live;
- make the feature target-neutral and deterministic across evaluation, LLVM, and direct Wasm;
- expose only raw-buffer representation primitives and implement `Vector` policy in Silk source.

**Non-Goals:**

- lifetime syntax or lifetime-generic APIs;
- results selected from multiple borrowed inputs;
- borrowed values in effects, services, captures, aggregates, errors, or other storage;
- compiler recognition of `Vector` or another standard-library actor.

## Decisions

### Returned views have exactly one declared source

The function contract records the ordinal of the borrowed parameter from which its result derives.
Every returned control-flow path must prove that same origin. A function whose result could derive
from different parameters is rejected. This keeps call-site substitution mechanical and avoids an
implicit lifetime-unification system.

An alternative was to infer a minimum lifetime across several parameters. That would make useful
programs depend on path-sensitive lifetime joins and would silently establish a much larger model,
so it is deferred.

### Returned borrows are restricted to ordinary functions

Effects, services, lazy values, and captures may outlive the activation that created them or store
their success value. Allowing a view through those boundaries would require lifetime parameters in
effect and service types. Ordinary calls have a direct caller-owned source and are the only admitted
boundary in this change.

### Provenance and liveness extend the existing ownership facts

An accepted view records its source place and access mode. The ownership phase computes the view's
lexical live range through its last use, suspends conflicting access to the source over that range,
and rejects any exit whose cleanup would invalidate the view. No runtime owner token is emitted;
the behavior is entirely static and target-neutral.

### Storage remains prohibited

Slice locals are admitted only as lexical views. Aggregate fields, arrays, unions, effects, errors,
and captures continue to require owned values. This prevents a returned view from becoming an
untracked long-lived value and keeps physical layouts lifetime-free.

### RawBuffer intrinsics expose representation, not Vector policy

Two unsafe target-neutral operations form shared and exclusive slices over a proven initialized raw
buffer range. Canonical `Vector.asSlice` and `Vector.asMutSlice` source validates the range and
supplies the public API. This follows the minimal-compiler-privilege rule and lets other ordinary
owners reuse the same representation primitive.

## Risks / Trade-offs

- Last-use analysis can produce confusing diagnostics if provenance is lost during lowering.
  Ownership facts and diagnostics will retain both the view formation and conflicting-access spans.
- The one-source rule rejects some safe conditional selection. The restriction is intentional until
  Silk has an explicit lifetime model.
- Unsafe raw-buffer primitives can expose uninitialized memory if wrappers violate their contract.
  Their sealed inventory and canonical safe wrappers keep that surface narrow and auditable.

## Migration Plan

This is additive. Existing call-scoped slice programs retain their behavior. Canonical `Vector`
source adopts the new accessors after all three engines accept returned-view fixtures. Rollback is
the removal of the new result contract, provenance facts, intrinsics, and wrappers; no serialized
user data or compatibility bridge is involved.

## Open Questions

None for this cut. Stored borrows, multiple origins, and lifetime-bearing effect results require
separate proposals.
