## Why

Nominal values cannot currently own named functions or capturing sections because their complete
types lose the callable identity needed for layout, invocation, ownership, and cleanup. This blocks
ordinary source from composing inspectable data with executable transformations.

## What Changes

- Admit callable-bounded representation parameters as nominal field value types after concrete
  specialization resolves one target and capture layout.
- Carry resolved callable field representations through construction, nesting, borrowing,
  invocation, whole-value moves, cleanup, HIR, layout, MIR, evaluation, native LLVM, and direct
  WebAssembly.
- Preserve shared `fn`, exclusive `mut fn`, and consuming `once fn` access rules through aggregate
  receivers.
- Keep representation-bearing nominals move-only for this milestone and reject direct owned field
  extraction; consuming invocation takes the whole aggregate.
- Retire `SEM0103` only for paths proven end to end through all three engines.

## Capabilities

### New Capabilities

- `bootstrap-nominal-callable-storage`: Static inline storage, access, ownership, cleanup, layout,
  lowering, and engine parity for callable fields.

### Modified Capabilities

- `bootstrap-callable-values`: Concrete callable environments may contribute inline layout through
  a representation-dependent nominal even though the structural callable contract has no standalone
  target layout.

## Impact

Depends on `introduce-representation-parameters`. Affects ownership, loans, cleanup planning,
target layout, HIR/MIR, evaluator, LLVM, direct Wasm, diagnostics, determinism, and differential
acceptance.
