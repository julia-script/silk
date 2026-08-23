## Context

The previous slices establish verified semantic facts, one package, drive, park, Wake, and cleanup
contracts. Current evaluation is the language oracle and native/Wasm already lower private nested
suspension. See `proposal.md` and the three delta specs.

## Goals / Non-Goals

**Goals:**

- Introduce one target-neutral verified transition model shared by every engine.
- Preserve execution-local logical roots and non-LIFO owner scheduling.
- Differentially verify every SLP ordering and cleanup path at the cheapest falsifying tier.

**Non-Goals:**

- Stabilize runtime ABIs, physical frame layouts, growth increments, or pooling.
- Add cross-thread execution, atomics, implicit roots, or canonical scheduling actors.

## Decisions

### Make MIR the complete authority boundary

Add purpose-specific operations/edges carrying package-plan identity, execution state, generation,
and authority flow. MIR validation proves legal predecessor states, callback metadata, exact branch
consumption, loan-safe cleanup, and final release before backends see the program. Inspection emits
logical identities and edges, never physical pointers or offsets.

Alternatives rejected: re-deriving legality separately in each engine invites semantic drift;
lowering an imperative runtime blob hides ownership and cleanup from verification.

### Model evaluation without JavaScript concurrency

The evaluator holds explicit execution records in its deterministic machine and runs one selected
activation until it completes or relinquishes. Scripted source/reactor actions consume logical Wake
values between drives. Trace identities are assigned canonically from construction and generation
order, not object identity.

Alternatives rejected: Promises/microtasks impose host scheduling and garbage collection behavior
not selected by Silk; recursive host calls cannot model execution-local depth reliably.

### Extend the existing continuation foundation per owner

Native and Wasm reuse current liveness, frame slots, resume labels, outcome restoration, and cleanup.
Nested-only frames retain their present LIFO owner. An explicit Execution supplies a distinct frame
stack/root and saved dispatch cursor that outlives a drive. External park saves into that owner and
returns through the verified suspension edge.

Alternatives rejected: a second coroutine lowering couples policy and duplicates safety-critical
machinery; grafting ready queues onto the thread-local LIFO stack cannot resume non-LIFO.

### Lower the wake cell as local state with abstract authority counts

Each backend chooses compact tags/flags and retain lanes sufficient for Execution, Wake,
Registering/notification preparation, and endpoint invocation. The initial implementation uses
ordinary local loads/stores because types forbid transfer. MIR's ordering, not hardware atomics,
defines the abstract transition.

Alternatives rejected: mandatory atomics violate pay-for-use; omitting transient authority makes
late Wake and reentrant destruction unsafe.

### Verify semantics cheaply and physical behavior selectively

Evaluation and Wasm cover every wake/cleanup case and relevant failure ordinal. Native cases enter
the designated differential corpus and cover target-specific boundaries: non-LIFO execution-owned
frames, traps, local reactor delivery, and release. Structural backend tests prove state checks and
cleanup dominance; correctness tests avoid timing, byte counts, and instruction counts.

Alternatives rejected: per-case native compilation inflates the critical path; timing tests cannot
prove semantic ordering; instruction-count assertions overconstrain target-private realization.

## Risks / Trade-offs

- **Shared continuation code regresses nested suspension** → retain existing nested corpus and add
  explicit artifact assertions that nested-only programs select no independent owner.
- **Evaluator accidentally relies on host GC** → model reclaim authority and release events
  explicitly and keep logical records independent of object finalization.
- **Backend state fusion hides an invalid transition** → require every fused tag set to map back to
  a validated logical state and inspect the mapping structurally.
- **Parity suite becomes expensive** → share one Analysis snapshot per source and use the prescribed
  evaluator/Wasm/native tiering and designated corpus.
