## Why

Public functions that construct capturing callables or Effects need a stable result contract even
when their concrete representation has no source-nameable item. Inferred public results and exposed
construction-site identities both make library boundaries unstable.

## What Changes

- Add exact `typeof(item)` representation arguments for fully specialized, sufficiently visible
  named callable items.
- Add contextual scoped `some<F: Contract> Result` binders for one declaration-owned opaque
  representation family.
- Separate the stable family key, versioned public signature, specialized family instance, and
  private realization/invalidation fingerprints.
- Require one realization per producer specialization and reject divergent returns, recursive
  realization cycles, private exact-identity leaks, and unresolved items.
- Keep opaque results static and monomorphic; they do not introduce existential packaging, runtime
  dispatch, or implicit allocation.

## Capabilities

### New Capabilities

- `bootstrap-opaque-representation-results`: Exact and opaque result syntax, visibility, equality,
  realization, privacy, recursion, and incremental invalidation.

### Modified Capabilities

None.

## Impact

Depends on `introduce-representation-parameters`. Affects parsing/formatting, public declaration
contracts, module dependency surfaces, HIR, specialization, layout dependencies, tooling, and
incremental invalidation.
