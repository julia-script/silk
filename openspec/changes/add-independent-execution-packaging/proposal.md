## Why

An independently owned execution needs stable erased-body and continuation storage, but hidden
runtime allocation would reverse SLP-0001's recoverable admission and explicit policy boundary.
This slice defines one caller-funded package and the drive/destroy mechanics over it.

Source: [SLP-0001, revision 31](../../../proposals/0001-independently-resumable-effect-executions/proposal.md),
SHA-256 `963a7420f16bce3bd0ec50acd906b4a4ee43319d132c5452d3686cde643c5635`,
realization slice 2 of 5. Depends on `establish-independent-execution-semantics`.

## What Changes

- Add exact `executionLayout<A, F, O, R>()` and unsafe
  `executionFromAllocation<A, F, O, R>(...)` operations that consume one matching Allocation, one
  Detached body, detached endpoint state, and one reusable Detached NonParking endpoint callback.
- Fix one combined Allocation as the owner record, erased body, exact invoke/drop metadata, fixed
  endpoint, optional wake storage, and optional initial continuation segment for the Execution's
  complete lifetime.
- Add callback-shaped, unit-returning `drive` with one affine branch state transferred to exactly
  one NonParking completion or suspension callback.
- Define never-driven, completion, suspension, dormant-destroy, and fatal illegal-state behavior;
  ordinary affine drop is destroy and post-construction stack growth remains fatal.
- Preserve typed failure by ordinary-source `Effect.result` and keep Allocator and
  `OutOfMemoryError` outside the intrinsic signatures.

## Capabilities

### New Capabilities

- `bootstrap-independent-execution-packaging`: define exact package construction, erased-body
  ownership, callback-shaped drive, execution-local roots, and package cleanup.

### Modified Capabilities

- `bootstrap-owned-allocation`: transfer one exact caller-funded Allocation into an indivisible
  Execution package and retain its private reclaim authority through every lifecycle path.
- `bootstrap-target-layout`: plan the exact combined target layout, including the zero-sized
  endpoint path and optional initial continuation storage.
- `bootstrap-intrinsic-boundary`: admit only the exact layout, unsafe initializer, and safe drive
  operations needed by ordinary wrappers.
- `bootstrap-ownership`: define consuming construction and drive transfers, exactly-one branch
  ownership, never-driven cleanup, completion cleanup, and dormant destroy.

## Impact

This affects target layout planning, Allocation provenance, semantic/HIR/MIR contracts, compiler
frame ownership, execution-local logical stacks, cleanup planning, and intrinsic diagnostics. It
depends on the semantic slice and precedes external wake parking; it adds no hidden allocator,
compiler-known safe wrapper, or recoverable post-construction stack failure.
