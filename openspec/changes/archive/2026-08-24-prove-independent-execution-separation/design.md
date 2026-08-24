## Context

The preceding slices provide the complete compiler and engine substrate. SLP-0002 provides local
Shared source state but deliberately stops before execution transfer. This final slice must prove
the substrate's source sufficiency and negative cost/privilege claims without selecting production
concurrency APIs. See `proposal.md` and the three delta specs.

## Goals / Non-Goals

**Goals:**

- Exercise every driving, boundary, cleanup, and alternate-owner case in connected ordinary source.
- Prove publication rollback and wake-time allocation behavior.
- Produce structural pay-for-use and actor-neutral privilege evidence.

**Non-Goals:**

- Ship canonical Scheduler, Fiber, Deferred, Timer, Coroutine, or root-adapter APIs.
- Measure performance with timing/size counts or decide parallel/structured concurrency policy.

## Decisions

### Build one small local Scheduler-shaped witness

Build a companion to the landed SLP-0002 `local-shared-slp1` pressure witness and extend its test
harness patterns, renamed-actor comparison, allocation-event projections, and failure-quota
machinery without replacing that independently runnable prerequisite witness. Use SLP-0002 Shared
state for distinct TaskStore and ReadyInbox actors. Reserve task and ready slots before publication.
The Execution endpoint owns only a cloned inbox handle plus TaskId, avoiding the strong cycle that
would result if it owned the TaskStore storing the Execution. Drive removes an Execution before
activation and holds no Shared access across drive or endpoint invocation.

Alternatives rejected: a compiler-known scheduler violates privilege; capturing the owning task
store leaks a strong cycle; a growable queue during wake hides allocation.

### Keep result and timer payloads in ordinary source state

The Deferred-shaped witness stores `Result<A,E>` and one Wake in Shared state. The timer witness
prepares fallible registration, then installs Wake in a same-thread reactor. Producers extract Wake
under short access and consume it after return. The explicit outer timer driver retains its reactor
and outer Execution separately.

Alternatives rejected: carrying payload in Wake broadens the intrinsic; worker-thread callbacks
prejudge transfer/atomic rules; blocking the owner does not prove external parking.

### Pressure an alternate owner with a bounded Coroutine shape

Use a shared port to hold `Yielded(value)` plus Wake. Each resume consumes Wake, waits for endpoint
publication to finish, then drives the Eligible Execution. The fixture verifies two yields,
completion, and drop while yielded. Names and API remain test-local.

Alternatives rejected: a production Coroutine API would require independent decisions about yield
typing, resume inputs, and borrowed payloads.

### Enumerate allocation ordinals before publication

Use failure injection around Shared cells, result state, waiter nodes, reservations, and exact
Execution packages. Evaluation and Wasm sweep every exercised ordinal; native runs boundary
ordinals through the designated acceptance corpus. Each case inspects publication and cleanup
events, not message text.

Alternatives rejected: one success/failure pair misses partial-publication windows; per-feature
native sweeps make the compiler suite needlessly expensive.

### Gate tiers with structural inventories

Compile five minimal programs sharing the same analysis helpers: ordinary direct, ordinary nested-
only, explicit direct, explicit nested-only, and explicit park-capable. Inspect reachable runtime slice identities and verified MIR
operations rather than byte or instruction counts. Add a renamed-policy fixture and a source-name
comparison inventory across compiler phases.

Alternatives rejected: artifact size is target/toolchain noise; imports cannot prove reachability;
passing behavior alone cannot prove absence of actor privilege or unused runtime machinery.

### Preserve the rejected smaller model as evidence

The findings report names the eager-start/owner-sweep/fatal-package alternative and maps three
pressure observations to the selected guarantees. No compatibility implementation or fallback is
kept because the repository is green-field and the accepted SLP chose the richer capability point.

## Risks / Trade-offs

- **Pressure actors accidentally look canonical** → keep them in pressure fixtures/findings and
  explicitly state their non-normative status in source and specs.
- **The final witness duplicates or weakens the SLP-0002 prerequisite evidence** → keep the landed
  local-shared witness independently runnable and reuse its harness/projection patterns from a
  companion Execution/Wake fixture.
- **Shared cycles invalidate cleanup evidence** → keep endpoint routing state separate from task
  ownership and add final-release/cancellation cases.
- **Tier assertions overfit backend layout** → inspect logical runtime slice and MIR identities only,
  never bytes, offsets, or instruction counts.
- **The source witness hides allocation through helpers** → trace every construction/growth point to
  declared Allocator/failure rows and assert no allocator access on park/wake/notify paths.
