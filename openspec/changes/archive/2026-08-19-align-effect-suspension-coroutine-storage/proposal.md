## Why

The implemented `Effect.suspend` contract contradicts the canonical
[effect-suspension rules](../../../../apps/docs/content/reference/effect-suspension.md): it exposes compiler
continuation allocation as `OutOfMemory ? &mut Allocator` and creates separate allocator-visible
records instead of using one reusable coroutine frame per suspendable invocation. Stabilization must
remove that accidental public policy before code and documentation teach it as a language rule.

## What Changes

- **BREAKING** Change `Effect.suspend<A, !E, ?R>` to `Effect.suspend<A, E, ?R>`, preserving exactly
  `A ! E ? R` while removing its obsolete failure-row binder, `OutOfMemory` failure, and exclusive
  `Allocator` requirement.
- Replace source-allocator-backed continuation records with compiler-owned execution-stack frames:
  one statically shaped reusable frame per concrete suspendable invocation and dynamic private
  storage proportional to active recursive invocations.
- Treat private execution-stack exhaustion as a fatal trap outside the typed Effect failure channel.
- Preserve ordinary ownership, stable borrows, provider access, typed outcomes, structured cleanup,
  logical `CallDepth`, cross-engine parity, suspension-transparent combinators, and zero coroutine
  cost for closed call graphs that cannot suspend.
- Remove allocator-observable continuation events, partial-chain rollback, captured reclaim
  authority, and the rule forbidding a selected continuation allocator from suspending.
- Update tests, Labs examples, diagnostics, and canonical language/API documentation to the accepted
  contract after implementation.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-flow-functions`: Preserve Effect channels exactly at suspension and define reusable
  per-invocation coroutine frames with fatal private execution-stack exhaustion.
- `bootstrap-intrinsic-boundary`: Remove allocation rows from the one target-neutral suspension
  primitive while keeping the public wrapper ordinary Silk and name-independent.
- `bootstrap-mir`: Replace allocator-visible relay continuation descriptors with reusable coroutine
  frame states and private execution-stack operations.
- `bootstrap-backend`: Realize the accepted frame/driver contract on native and direct Wasm without
  source allocator calls or typed storage failure.
- `bootstrap-evaluation`: Model coroutine frames and fatal execution-storage exhaustion while
  retaining deterministic logical `CallDepth` and trace behavior.
- `bootstrap-owned-allocation`: Remove continuation frames from the source `Allocator` and
  `OutOfMemory` contract.
- `bootstrap-ownership`: Preserve affine values, stable borrows, providers, and exact cleanup across
  reusable frame states without allocation-prefix rollback or allocator reclaim obligations.

## Impact

The change affects the shipped Effect source, intrinsic catalog contracts, suspendability analysis,
MIR frame/state planning and verification, evaluator activations and traces, native LLVM and direct
Wasm lowering, ownership/cleanup planning, allocator-specific diagnostics, suspension fixtures and
cost checks, Labs examples, and language/API documentation. Existing source that provides an
allocator or handles `OutOfMemory` solely for `Effect.suspend` must remove that scaffolding. No
scheduler, fiber, parking, wakeup, cancellation, or ordinary-recursion guarantee is introduced.
