## Context

After representation parameters exist, a complete nominal identity can name one callable target and
capture environment. Existing ownership, layout, and lowering still treat only direct callable
parameters and environments specially; nominal fields need one shared resolved representation fact.

## Goals / Non-Goals

**Goals:**

- Store named and capturing callables inline in move-only nominals.
- Preserve invocation access, capture loans, layout, cleanup, and static targets through all engines.
- Narrow `SEM0103` only for complete, proven paths.

**Non-Goals:**

- Structural nominal Copy, partial owned field extraction, heterogeneous callable collections, or
  uniform closure allocation.
- Runtime interface dispatch or WebAssembly tables.

## Decisions

### Resolve one field representation per specialized nominal

Generic analysis records a symbolic field plan keyed by representation binder and field ordinal.
Instance specialization resolves it to target, concrete generic arguments, ordered capture slots,
invocation access, loan dependencies, liveness, and cleanup. Ownership, layout, MIR, and engines must
consume this shared fact rather than rediscovering callable construction from syntax.

The first task is a vertical slice for one named callable and one affine capturing section. Work
stops if any phase needs a parallel representation model.

### Keep representation-bearing nominals move-only

This milestone does not generalize nominal Copy. Shared and exclusive borrows preserve field access;
whole-value moves transfer the complete environment. Direct owned extraction of a representation
field is rejected, avoiding path-sensitive residual cleanup. `once fn` invocation consumes the whole
aggregate.

### Derive invocation from receiver access

Shared receiver access admits `fn`; exclusive access also admits `mut fn`; whole-owner take access
also admits `once fn`. The callable argument's own parameter ownership remains independent.

### Store capture environments inline

The structural callable contract remains unlayoutable. A concrete resolved field contributes its
capture slots to the enclosing build-internal nominal ABI. MIR carries aggregate paths plus one
static target and cleanup plan. Evaluator, LLVM, and direct Wasm consume the same MIR decision.

### Retire diagnostics case by case

Remove `SEM0103` only when analysis, ownership, layout, MIR, evaluator, LLVM, and Wasm support the
concrete path. Unknown identity remains an identity-loss error; known inequality uses the join
diagnostic. No evaluator-only support counts as completion.

## Risks / Trade-offs

- [Capture cleanup can double-run] → Forbid field extraction and test uncalled, called, moved, and
  typed-failure exits with exact traces.
- [Borrowed captures escape through aggregates] → Carry loan dependencies on the resolved field and
  reuse scoped-escape checking.
- [Backend layouts diverge] → Derive all engines from canonical target layout and compare traces and
  emitted static targets.

## Migration Plan

1. Implement the field-representation vertical slice while all public fences remain.
2. Integrate ownership, loan, liveness, and cleanup facts.
3. Add target layout and MIR aggregate callable operations.
4. Add evaluator, LLVM, and direct-Wasm parity.
5. Narrow `SEM0103` only for the accepted matrix and add negative fences for every other path.

Rollback re-enables the fence; source syntax remains owned by the prerequisite representation change.
