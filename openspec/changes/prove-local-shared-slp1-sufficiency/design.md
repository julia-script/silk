## Context

SLP-0001 owns execution transfer and parking, while SLP-0002 owns the local shared-state prerequisite.
This slice is evidence, not a second Scheduler proposal. The repository already uses complete
ordinary-Silk pressure programs and findings reports to decide whether general language work is
sufficient. See `proposal.md` and the pressure-program delta.

## Goals / Non-Goals

**Goals:**

- Prove independently retained local callbacks can share one ready inbox without an escaping `&mut`.
- Prove producer and waiter handles can share one Deferred-style state with exact affine cleanup.
- Demonstrate that external callbacks need not run while shared access is active.

**Non-Goals:**

- Implement SLP-0001 execution objects, transfer, parking, wake order, Scheduler policy, or public Deferred API.
- Promote the witness actors into compiler-known or canonical production actors.

## Decisions

### Build one connected fixed-capacity ordinary-Silk witness

The pressure source defines a fixed-capacity `ReadyInbox`, a `DeferredState<A>` with a bounded waiter
collection, and small wrapper actors under deliberately non-privileged source names. Multiple
dormant ordinary callables and Effects retain explicit cloned handles to the same inbox and state;
the witness includes both a stored callable and an unrun Effect so capture retention and
dropped-Effect cleanup are exercised.
Fixed capacity keeps every `withMut` allocation-free; construction performs the only required
allocations through ordinary `Shared.make`.

Alternatives rejected: a growable queue would mix allocator pressure into the access proof; using
SLP-0001 execution transfer before its own handoff would reverse dependencies; installing Deferred
as canonical stdlib would turn a sufficiency test into a premature API commitment.

### Extract callbacks before invoking external code

Registration mutates waiter state under one short `withMut`. Publication moves the offered affine
value into the shared state exactly once, moves only the registered readiness callbacks into ordinary
owned locals under `withMut`, returns and restores access, then invokes those callbacks. The value
remains source-owned by shared state so every waiter handle can observe it through a short shared
borrow. Ready callbacks similarly enqueue identifiers under a short access and return.
No source path calls an unknown callback while holding control-block access, so the all-exclusive
reentrancy matrix is sufficient and needs no reader count or lock guard.

The connected witness performs one publication call and verifies one state transition, one retained
value owner, and one notification per registered waiter. Repeated-publication result policy belongs
to a future Deferred API and is not inferred by this evidence slice.

### Make the findings gate explicit

The checked-in findings report records every observed wall as language, standard-library,
compiler-defect, tooling/ergonomics, or performance/cost; cites concrete evidence; records repaired,
deferred, or accepted disposition and the smallest plausible follow-up; compares repeated findings
with the lexer and stack-VM pressure reports; and selects any next work from evidence rather than a
predetermined roadmap. It also records whether the witness can be written using only general
language and stdlib operations. A passing result requires no compiler phase or MIR/backend branch to
name the witness actors, `Shared`, Deferred, Scheduler, queue, or callback registry. Renaming the
source actors must leave semantic and runtime behavior unchanged.

Cross-engine cases cover two dormant ready callbacks, one-time publication with several waiters,
dropping an unpublished affine payload, dropping a dormant callback before publication, exact final
release, and deterministic subsequent runs. Native coverage goes through the designated acceptance
corpus; evaluator and Wasm cover every relevant failure ordinal.

## Risks / Trade-offs

- **Risk: the witness accidentally tests execution transfer not shared state** → retain dormant
  work in existing ordinary callable/Effect values and state clearly which SLP-0001 behavior remains absent.
- **Risk: actor names leak into compiler behavior** → include rename and artifact-inventory audits.
- **Risk: fixed capacity hides a needed allocation interaction** → record capacity as deliberate
  isolation; growth remains ordinary callback policy and is not required to prove the ownership wall removed.
