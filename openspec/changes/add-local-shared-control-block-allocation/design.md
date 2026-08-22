## Context

`Allocation` already carries private self-contained reclaim authority and `Layout` already captures
target-sized validated size and alignment. The local shared control block must reuse that model while
giving ordinary source an exact allocation request. See `proposal.md` and the delta specs.

## Goals / Non-Goals

**Goals:**

- Keep allocation choice and `OutOfMemoryError` in ordinary source.
- Transfer one allocation and one `T` atomically into an initialized control block.
- Preserve a target-neutral logical contract while allowing target-specific physical layouts.

**Non-Goals:**

- Expose raw addresses, reclaim tickets, count lanes, or public deallocation.
- Implement clone, access, or drop transitions; the lifecycle slice owns them.

## Decisions

### Plan one opaque target-specific control-block layout

`sharedLayout<T>()` asks target layout planning for one opaque block containing the logical fields:
strong count, access state, private reclaim state, and initialized `T`. The returned ordinary
`Layout` includes all header padding and alignment, is nonzero even for zero-sized `T`, and is the
exact request accepted by `sharedFromAllocation<T>`. Physical field order and reclaim representation
remain backend-private; the canonical layout fact retains the target, concrete `T`, size, alignment,
and a provenance identity tying the two operations together.

The intrinsic is available only when the selected target can represent the complete concrete block.
Header addition, alignment rounding, or payload placement overflow makes that specialization's
target-layout fact unavailable and rejects the reachable call before MIR or execution, retaining a
stable diagnostic at the `sharedLayout<T>` call. It is not runtime `LayoutOverflow` data because no
runtime repetition is being requested, and it is not a trap or an allocation failure; therefore the
accepted total source signature gains no failure branch.

Alternatives rejected: a universal byte formula would either expose private lanes or be wrong on a
target; allocating inside `sharedFromAllocation` would hide the allocator dependency; a `RawBuffer`
header would publish unsafe storage power broader than this sealed ownership use case.

### Consume allocation authority into the opaque block

`sharedFromAllocation<T>` requires an active `Allocation` proven to originate from the exact
`sharedLayout<T>` fact. It consumes that owner and `value: T` in one unsafe initialization
transition, sets strong count to one and access to available, installs the private reclaim authority,
and returns one initialized core. No intermediate core or separately live payload becomes visible.

The operation is unsafe because source must uphold exact layout provenance and one-time transfer.
The compiler still checks canonical type/layout identity, active owner liveness, and consuming
arguments. A violation of runtime initializedness after bypassing those checks remains an unsafe
contract violation, not a typed construction failure.

### Leave allocation failure entirely before initialization

Ordinary source calls the selected allocator first. If it returns `OutOfMemoryError`,
`sharedFromAllocation` is not invoked, no control block or reclaim obligation exists, and ordinary
typed-failure cleanup retains and cleans `T` exactly once. Once the unsafe initializer is called
with valid inputs it has no failure channel and cannot publish partial state.

## Risks / Trade-offs

- **Risk: layout and initializer select different concrete specializations** → carry one canonical
  provenance identity through semantic facts and verified MIR; reject mismatches before execution.
- **Risk: backend-private reclaim metadata changes size or overflows the target word** → make the
  target layout planner the sole source of the public `Layout` result, reject an unrepresentable
  specialization before MIR, and compare committed layout facts rather than hard-coded offsets.
- **Risk: initialization defects leak an allocation or `T`** → express one consuming MIR transition
  and verify it before any engine lowers the operation.
