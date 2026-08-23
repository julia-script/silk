## Why

An initialized local-shared core is useful only if the compiler can duplicate its dynamic lifetime,
confine access, and choose one last cleanup authority. This slice defines those operations as one
coherent ownership lifecycle.

Source: [SLP-0002, revision 6](../../../proposals/0002-allocation-backed-local-shared-ownership/proposal.md),
SHA-256 `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`,
realization slice 3 of 6. Depends on `establish-local-shared-ownership` and
`add-local-shared-control-block-allocation`.

## What Changes

- Add allocation-free `sharedClone` with a target-bounded strong count and fatal overflow before mutation.
- Add one callback-shaped exclusive `sharedWithMut` primitive that invokes exactly one of `use` or
  `onConflict`; conflict observation leaves the active access unchanged.
- Reject callback-borrow escape through direct results, generic results, Effects, stored callables,
  or suspension.
- Make strong-count state independent of access state, allowing clone and non-last drop during access.
- Define non-last decrement and last-handle cleanup of `T` followed by allocation release exactly once.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-ownership`: track clone/drop obligations, callback-scoped access loans, escape rejection, and exact last-handle cleanup.
- `bootstrap-intrinsic-boundary`: admit the clone and callback access primitives and their closed contracts.

## Impact

This affects ownership facts, cleanup plans, callable-borrow checking, intrinsic typing, MIR
operations, traps, and diagnostics. It deliberately adds neither a shared-reader primitive nor
atomics, locks, weak handles, cycle collection, or Effect-returning access callbacks.
