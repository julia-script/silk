## Why

Effect values have the same representation-loss boundary as callables: a structural success,
failure, requirement, and run-access contract does not determine the runner or captured environment
needed by an enclosing nominal value.

## What Changes

- Add shared `Effect`, exclusive `mut Effect`, and consuming `once Effect` representation bounds.
- Store one concretely realized Effect runner and environment inline in a representation-dependent
  nominal without running it.
- Preserve exact rows, run access, captures, suspension state, nesting, loans, whole-value moves,
  and cleanup through evaluation, LLVM, and direct Wasm.
- Keep the structural Effect contract outside the standalone executable ABI while making the
  concrete environment part of the enclosing build-internal nominal ABI.
- Retire the unavailable-Effect-layout fence only for cross-engine-proven paths.

## Capabilities

### New Capabilities

- `bootstrap-nominal-effect-storage`: Static inline Effect storage, access, ownership, cleanup,
  suspension-aware invalidation, layout, lowering, and engine parity.

### Modified Capabilities

- `bootstrap-callable-values`: Effect identity and environment preservation extends through
  representation-dependent nominal fields.

## Impact

Depends on `introduce-representation-parameters` and reuses the field-representation substrate
proven by `store-callables-in-nominals`. Affects flow typing, ownership, layout, HIR/MIR, suspension
lowering, evaluator, LLVM, direct Wasm, and deterministic invalidation.
