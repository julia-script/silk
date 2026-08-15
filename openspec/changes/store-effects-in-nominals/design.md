## Context

Effect values already have construction-site identities, runner functions, captured environments,
typed rows, run access, and optional suspension state. Their structural contracts are deliberately
not standalone ABI values. The callable-storage substrate provides the nominal field path that Effect
storage must reuse.

## Goals / Non-Goals

**Goals:**

- Store one concrete Effect realization inline without running it.
- Preserve rows, run access, suspension, loans, and cleanup through all engines.
- Retire Effect layout fences only for proven cases.

**Non-Goals:**

- Give `Effect<A ! E ? R>` a uniform standalone ABI.
- Run an Effect during storage, hide multiple runners, or add backend-specific representations.
- Add runtime row dictionaries or service behavior to interfaces.

## Decisions

### Add explicit Effect access bounds

Use `Effect`, `mut Effect`, and `once Effect` parallel to callable access. Shared realizations satisfy
all weaker-use bounds; exclusive realizations satisfy consuming bounds; reverse substitutions fail.
Generic-body admissibility follows the aggregate receiver access.

### Reuse the resolved field representation

Extend the callable field fact with a tagged Effect realization containing runner, concrete generic
arguments, ordered environment slots, run access, rows, cleanup, and suspendability. Ownership,
layout, MIR, and engines consume the same fact. Work stops if a backend requires a separate Effect
field model or reconstructs runner behavior.

#### Publish one construction fact and extend the shared field index

Instance discovery publishes one `EffectInstance` for each concrete source Effect construction. It
contains the canonical runner identity and target, the enclosing concrete arguments, ordered typed
captures, and suspendability. HIR owns the single projection from an Effect site to the semantic
origin retained by exact representations; realization and lowering delegate to that projection
rather than rebuilding its encoding. The retained origin also carries the enclosing executable's
specialization, so two concrete instantiations of the same generic source site select different
runner facts. Captured local Effect and callable bindings publish their already-resolved nested
identities with the ordered environment; later phases never recover them from binding initializers.

`CallableFieldRealization` remains the one resolved-field index and now carries a tagged union. Its
Effect tag enriches the existing `RepresentationField` identity with the discovered runner, exact
contract rows, actual run access, ordered environment, unrun cleanup lanes, and suspendability. It
contains no sizes, offsets, row dictionaries, or dispatch ABI. Callable consumers explicitly narrow
to the callable tag, and no Effect consumer is enabled in this slice, so `SEM0107` still stops every
stored Effect before layout and MIR.

The task-2 cleanup and suspension cases are realization proofs, not runtime claims. The unrun case
records the owned lanes that later ownership and cleanup phases must release; the suspension case
records the exact runner's transitive suspendability. Executing either case remains work for the
ownership/layout/MIR and engine slices below.

### Keep Effects lazy and inline

Construction transfers or borrows captures into the nominal but never enters the runner. The
concrete environment contributes lanes to the enclosing build-internal ABI; the structural Effect
contract remains lane-less. Whole-owner moves and run access transport the entire realization.

### Include suspendability in realization invalidation

Runner target, capture shape, access, cleanup, and suspendability are independent invalidation
inputs. A suspendability change must rebuild affected MIR and engine artifacts even if ordinary
capture lanes stay equal.

### Require cross-engine cleanup parity

An unrun Effect, successful run, typed failure, suspension/resume, and scope exit must clean each
live capture once in evaluator, LLVM, and Wasm. Direct owned field extraction remains forbidden.

## Risks / Trade-offs

- [A backend tries to materialize a standalone Effect ABI] → Gate the vertical slice on consuming
  the enclosing resolved representation only.
- [Suspension state silently changes layout] → Fingerprint suspendability and compare fresh-process
  realization facts.
- [Rows leak into runtime] → Keep rows in specialization keys and contracts, never layout lanes.

## Migration Plan

1. Add Effect access-bound syntax and kind checking.
2. Complete the stored-Effect vertical slice for unrun cleanup and one suspension.
3. Integrate ownership, layout, MIR, and evaluator.
4. Add LLVM/direct-Wasm parity and invalidation fixtures.
5. Narrow the Effect-layout fence only for passing shapes.

Rollback re-enables the fence without changing ordinary direct Effect behavior.
