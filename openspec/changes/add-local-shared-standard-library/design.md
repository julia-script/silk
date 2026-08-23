## Context

The prerequisite changes provide four sealed operations and opaque-core cleanup. Project policy
requires public reusable policy to live in canonical Silk source and forbids recognizing library
actors by spelling. See `proposal.md` and the `bootstrap-silk-stdlib` delta.

## Goals / Non-Goals

**Goals:**

- Expose explicit construction, clone, shared inspection, and exclusive mutation safely.
- Keep allocation failure and conflict policy in visible source.
- Make the module a renameable ordinary consumer of sealed primitives.

**Non-Goals:**

- Add identity, downgrade, Weak, cycle management, thread-safe sharing, or recoverable conflict APIs.
- Hide allocation behind clone/access or let an access callback return an Effect-held borrow.

## Decisions

### Store exactly one opaque core

Canonical `silk/shared` defines `Shared<T>` with one private `Intrinsic.SharedCore<T>` field. The
nominal receives its affine category, local affinity, and cleanup recursively from that field; it
declares no compiler-special Drop hook and earns no intrinsic nominal entry of its own.

Alternatives rejected: storing a raw address would discard provenance and last-drop authority;
adding a source count beside the core would create competing state; compiler-known `Shared` would
make renaming and alternate safe wrappers impossible.

### Make construction the only allocator-bearing public operation

`Shared.make<T>(value)` calls `Intrinsic.sharedLayout<T>()`, requests that layout through the
ordinary `Allocator` service, and invokes unsafe `sharedFromAllocation` only after success. Its
contract is `Effect<Shared<T> ! OutOfMemoryError ? &mut Allocator>`. On allocation failure the
ordinary Effect frame still owns and cleans `value`; after initialization the returned core owns it.

`clone`, `with`, and `withMut` are synchronous, allocation-free, and have no failure or service row.
The callback may itself perform explicit ordinary allocation, but the wrapper introduces none.

### Derive shared inspection from the exclusive primitive

`Shared.withMut` delegates to `sharedWithMut`; its `onConflict` callback invokes the existing general
fatal-trap surface with source-owned policy. `Shared.with` delegates through `Shared.withMut`,
narrows `&mut T` to `&T` for the user's callback, and therefore reuses the same source conflict
policy. All four reentrant public combinations trap and no separate reader state exists.

Both APIs accept take-once ordinary callbacks. Existing restricted-borrow checking prevents their
parameters from escaping. A later source wrapper may translate `onConflict` to result data without
changing the intrinsic, but this slice does not ship one.

### Keep external invocation outside access regions

Documentation and source witnesses use the pattern “mutate/extract under `withMut`, then call
external callbacks after return.” `Shared` does not attempt to detect higher-level reentrancy or
provide a lock guard. This makes the initial all-exclusive state sufficient for ready-inbox and
Deferred actors.

## Risks / Trade-offs

- **Risk: generated shipped-source metadata drifts** → update the manifest and regenerate the
  canonical source table in the same task, then test source resolution through the public module.
- **Risk: public docs imply nested reads are permitted** → document the four-way conflict matrix and
  include a trap acceptance case.
- **Risk: compiler privilege slips in during integration** → audit intrinsic registries and phase
  branches for the public names and prove an equivalent renamed source wrapper works.
