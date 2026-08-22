# SLP-0003: Implicit ownership for park-capable Effect entries

SLP: 0003
Status: Draft
Revision: 1
Author: Julia Ortiz
Created: 2026-08-22
Updated: 2026-08-22
Discussion: —
Review record: —
Depends on: [generics, interfaces, and specialization](../../docs/language/generics-interfaces-and-specialization.md), [unsafe code, intrinsics, and targets](../../docs/language/unsafe-intrinsics-and-targets.md), [runtime and standard-library boundary](../../docs/language/runtime-and-standard-library.md), [Effect suspension](../../docs/language/effect-suspension.md), SLP-0001
Split from: SLP-0001
Split into: —
Supersedes: —
Superseded by: —
Revisit when: —
Resolution: —
OpenSpec handoff: —

## Summary

Silk should decide whether a complete program entry whose specialized Effect can externally park is
implicitly adapted into a root `Execution` owned by the target entry boundary. The desired source
experience keeps ordinary `run` and ordinary service provision; it does not introduce a public
executor or concurrency-specific run method. The independent decision is which synchronous entry
contracts may wait or poll until that execution completes, how root execution and readiness storage
are funded, which targets support the contract, and how rejection differs from an asynchronous host
whose call must return before completion.

## Problem and evidence

SLP-0001 lets ordinary source construct, drive, park, wake, and destroy an explicit independently
resumable Effect execution. A Scheduler can therefore own parked children. At a complete program
entry, however, there is no ordinary source caller outside `main` to receive the suspended execution.
If an evented `Timer.sleep` parks there, either entry adaptation owns the unfinished root, the target
blocks in some unrelated provider call, or the program has no defined continuation owner.

This is independent from fibers. A program with no Fiber or Scheduler can still need a root event
wait. It is also independent from the explicit Execution substrate: an ordinary-source scheduler can
use SLP-0001 even if a target declines to support implicit park-capable entries.

## Driving examples: current and desired

### Case: Sleep at the complete program root

#### Intent

Wait for a system timer and continue in the same Effect without constructing a Fiber or naming an
executor.

#### Current Silk

```silk,ignore
effect fn delayedValue() -> i32
! TimerError | OutOfMemoryError
? &Timer {
  run Timer.sleep("1 second")
  return 42
}

pub effect fn main() -> i32
! TimerError | OutOfMemoryError {
  let timer = EventedTimer.make()
  return run delayedValue()
    |> Effect.provide<Timer>(&timer)
  // BLOCKED: if sleep externally parks, no explicit owner exists outside main.
}
```

SLP-0001 deliberately stops at that ownership boundary. `Effect.suspend` cannot solve it because its
known nested child must complete before the direct parent resumes; it neither registers a system
wake nor leaves the root dormant.

#### Desired Silk

```silk,ignore
effect fn delayedValue() -> i32
! TimerError | OutOfMemoryError
? &Timer {
  run Timer.sleep("1 second")
  return 42
}

pub effect fn main() -> i32
! TimerError | OutOfMemoryError {
  let timer = EventedTimer.make()
  return run delayedValue()
    |> Effect.provide<Timer>(&timer)
}
```

The source remains ordinary. After provider selection and specialization prove external parking
reachable, a compatible synchronous entry adapter creates and owns the root Execution, drives it,
retains it after parking, waits or polls for readiness without the intrinsic itself driving from
Wake, drives the Eligible root again, cleans it on termination, and returns its final outcome through
the existing synchronous entry contract.

The exact source-visible storage contract is the Draft's principal unresolved frontier. Candidate
preparation must choose whether target entry resources fund fixed root execution/readiness packages,
whether the complete entry must close an explicit allocation requirement for them, or whether a
third target-neutral contract is necessary. The choice must not hide ordinary library allocation
merely for aesthetic reasons.

#### Observable result

On a compatible synchronous target, the entry waits without losing the timer notification and
returns `42`. Code after sleep is a resume state, and values live across it retain the same ownership,
loan, provider, failure, logical-depth, and cleanup semantics as an explicit SLP-0001 Execution.

#### Boundary case

```silk,ignore
// Selected target's entry ABI must return to its embedding before the timer can fire.
pub effect fn exportedMain() -> i32
! TimerError | OutOfMemoryError {
  let timer = EventedTimer.make()
  return run delayedValue()
    |> Effect.provide<Timer>(&timer)
}
```

This proposal must not manufacture a synchronous return, Promise, callback, or event-loop bridge.
Unless the selected target has a compatible synchronous root owner, the complete entry is rejected
after specialization. Returning before root completion requires a later asynchronous host contract.

### Case: Keep explicit child ownership independent

#### Intent

Ensure root adaptation is not a prerequisite for every use of parking.

#### Current Silk

Ordinary source cannot yet construct an independently resumable execution.

#### Desired Silk

```silk,ignore
let execution = run Execution.make(closedChild)
run Scheduler.enqueue(move execution)
```

#### Observable result

The source Scheduler owns and drives the child under SLP-0001. A target that rejects implicit
park-capable entries may still compile this explicit ownership path when the complete outer entry
itself never parks.

#### Boundary case

Merely calling `run closedChild` at the entry does not implicitly select Scheduler or transfer the
child to its task store. Entry ownership, source Scheduler ownership, and nested `Effect.suspend`
remain distinct policies.

## Goals and non-goals

### Goals

- Decide whether executable entry adaptation is the implicit root Execution delimiter.
- Preserve ordinary `run` and ordinary provider elimination as the source experience.
- Specify the complete synchronous root lifecycle: construction, first drive, parking, readiness,
  later drive, completion, fatal termination, and cleanup.
- Select an honest root storage and exhaustion contract.
- Reject unsupported complete entries after provider selection and specialization.
- Reuse SLP-0001's continuation, Execution, Wake, eligibility, logical-depth, and destroy semantics.

### Non-goals

- Add `LocalExecutor`, `Scheduler.run`, or another concurrency-specific source entry operation.
- Select Fiber, Scheduler, Timer, Deferred, or reactor APIs.
- Define asynchronous return-before-completion, Promise conversion, callbacks, exported root
  handles, host cancellation, or a browser/event-loop ABI.
- Change the meaning of ordinary nested `Effect.suspend`.
- Give source-created child executions an implicit or hidden allocation policy.
- Define parallel execution, thread transfer, atomics, or work stealing.

## Current language model

Executable entry adaptation validates a complete specialized Effect and maps its final success,
typed failure, fatal trap, and requirements into the selected target contract. It owns the root
machine stack but does not yet own a source-visible independently resumable Execution. SLP-0001 adds
the explicit Execution substrate while intentionally declining to select this implicit adapter.

## Proposed language model

The provisional direction is one implicit boundary at complete executable entry, not at every
ordinary `run`. When the specialized entry cannot externally park, existing direct or nested-only
lowering remains unchanged. When it can park, a compatible target adapter owns one root Execution
and applies the same state machine as an ordinary SLP-0001 owner.

```text
complete specialized entry
  |-- external park unreachable -> existing direct or nested-only entry
  `-- external park reachable
        |-- compatible synchronous root owner -> implicit root Execution lifecycle
        `-- no compatible owner                -> compile-time target rejection
```

The adapter is compiler/toolchain behavior, not a provider implicitly inserted into the entry's
requirement row. It recognizes neither Timer nor Scheduler by spelling; reachability of sealed
`Intrinsic.park` after provider specialization is the relevant fact.

## Worked language experience

Candidate preparation must add complete cases for:

- early timer fire during registration followed by root suspension handoff;
- late timer fire after the root is dormant;
- root completion without ever taking its statically reachable park branch;
- typed failure before and after a park;
- fatal termination while dormant;
- externally requested process termination or target shutdown while the root is dormant;
- borrowed root providers whose lexical lifetime is the complete synchronous entry;
- a target that supports explicit child Executions but rejects implicit root parking; and
- a target/embedding that requires asynchronous outcome delivery and therefore remains out of scope.

## Semantic sketch

1. The compiler closes requirements and specializes the complete entry before classifying external-
   park reachability.
2. A non-parkable entry uses the existing direct or nested-only entry path.
3. A parkable entry is valid only for a target with the selected synchronous root-owner contract.
4. The adapter constructs root execution and readiness storage under the storage policy this Draft
   must select, then drives the root as Initial.
5. Completion maps the final value or reified program outcome to the existing entry contract.
6. Parking transfers the root Execution to adapter ownership; readiness makes it Eligible but never
   causes the SLP-0001 intrinsic itself to drive it.
7. A later adapter turn redrives the Eligible root. Waiting or polling is target policy constrained
   by observable ordering and cleanup, not a portable source API.
8. Termination destroys dormant state exactly once and respects SLP-0001's Wake cancellation and
   whole-readiness-allocation lifetime.
9. An unsupported target reports one deterministic compatibility diagnostic before lowering.

## Compiler–standard library boundary

### Compiler necessity

Ordinary source cannot place code outside the complete entry to retain its unfinished Execution,
control the machine-entry return, or select a target's wait/poll integration. Those powers belong to
entry adaptation rather than a standard-library Scheduler.

### Smallest target-neutral primitive

No additional source-callable intrinsic is provisionally required. The adapter can consume
SLP-0001's compiler-private execution lifecycle internally. Candidate preparation must prove that
the chosen root storage contract does not require another source-visible primitive or silently
special-case a library allocator.

### Standard-library construction

Timer and other event providers remain ordinary source over target-specific event primitives. They
use SLP-0001 `park`/Wake behavior identically inside a root or explicit child. No standard-library
actor is the root owner by privileged name.

### Privilege audit

Making Scheduler or Timer compiler-known is too large. Requiring a public executor/run wrapper is
source-expressible but changes the desired entry experience and makes root-only event waits depend on
a concurrency API. Treating every `run` as an owner is also too large: it would bubble independent
parking machinery through ordinary frames instead of placing one delimiter at executable entry.

## Whole-language interaction map

| Surface | Disposition | Analysis |
| --- | --- | --- |
| Syntax and names | Not affected — provisional | Ordinary `run`, provider calls, and entry declarations remain unchanged. |
| Types and abstraction | Affected | Complete-entry reachability selects root ownership; no public root type is currently proposed. |
| Execution contracts | Affected | A synchronous entry may remain unfinished while parked but cannot return before final completion. |
| Ownership and resources | Unknown — storage decision gate | Root package procurement, borrowed root providers, Wake cancellation, and termination cleanup must be fixed. |
| Runtime and targets | Affected | Entry adapters need drive/wait-or-poll/redrive and target compatibility contracts. |
| Compiler | Affected | Complete specialization, park-capability reachability, entry lowering, diagnostics, and cleanup participate. |
| Standard library | Not affected — policy remains ordinary | Timer and other providers use SLP-0001 without becoming root owners by name. |
| Tooling and diagnostics | Affected | Unsupported park-capable entry must differ from open requirements and unsupported provider intrinsics. |
| Learning and use | Affected | Users need to understand that root support belongs to the selected target, while Fiber operations belong to Scheduler. |

## Scope cohesion

This SLP asks one question: should complete executable entry adaptation implicitly own an externally
parkable root Effect, and under what synchronous target and storage contract? It is split from
SLP-0001 because explicit source ownership works without this adapter and because root ABI,
validation, and storage can coherently reach a different outcome.

Asynchronous host integration is a later proposal: returning before completion introduces a distinct
programmer concept and outcome-delivery ABI. Timer APIs are also independent; they merely pressure
the root owner through SLP-0001's common parking protocol.

## Complexity and subtraction budget

The provisional direction adds no source syntax, public type, standard-library actor, or sealed
operation. It adds one conditional entry-lowering contract and target capability. The storage
decision must account for every implicit byte and failure before this Draft can claim that budget.

## Surface displacement

The adapter displaces a public executor/run wrapper only for complete synchronous entries. It does
not displace explicit Execution owners, Scheduler policy, Timer providers, or asynchronous host APIs.

## Drawbacks and risks

- Invisible entry adaptation can obscure real execution/readiness storage cost unless procurement is
  stated precisely.
- A synchronous wait/poll contract may not fit embeddings whose calls must return promptly.
- Target-dependent acceptance can surprise users when the same source selects a different provider
  or target.
- Borrowed root providers need a proof that adapter lifetime never exceeds the complete synchronous
  entry invocation.
- Overgeneralizing the adapter could impose independent-execution machinery on direct entries.

## Alternatives and prior art

### Status quo

Require complete entries to be externally non-parkable. Explicit Scheduler-owned children still
work, but root-only evented sleep is rejected everywhere.

### Smaller primitive or library solution

Require users to construct an executor and call a special run method around every park-capable root.
This needs no implicit adapter, but adds a concurrency concept to programs that only wait for one
system event and contradicts the desired ordinary `run` experience.

### Strongest competing language model

Make every Effect universally pollable and let every `run` propagate Pending to an ambient runtime.
That unifies root and child execution but changes ordinary Effect composition, broadens the runtime
representation, and weakens SLP-0001's explicit construction delimiter and pay-for-use tiers.

## Falsifiers and acceptance blockers

- A non-parkable entry gains Execution, Wake, wait/poll, or readiness-package machinery.
- Root parking requires a public executor or special run operation despite the selected source goal.
- An unsupported target accepts a parkable entry and then blocks incorrectly, returns early, or
  loses readiness.
- The adapter recognizes Timer, Scheduler, or another library declaration by spelling.
- Root storage allocates or can fail without the selected contract making that cost explicit.
- A borrowed root provider outlives the synchronous entry invocation.
- Root wake drives the continuation before suspension handoff is complete or violates SLP-0001's
  eligibility state machine.
- Dormant termination leaks or double-cleans captures, providers, registration guards, Wake state,
  or readiness Allocation.
- The proposal silently defines asynchronous result delivery while claiming only synchronous entry.

## Open realization questions

- Which root storage procurement model best preserves explicit allocation without requiring a public
  concurrency runner?
- Which current executable entry contracts can synchronously wait or poll, and how is that capability
  represented in target validation?
- How do process termination and fatal target shutdown destroy a dormant root under Silk's no-unwind
  rules?
- What diagnostic distinguishes unsupported root ownership from an unavailable reachable Timer
  intrinsic?

## Future directions

A later host-integration SLP may define asynchronous exported roots, outcome delivery, cancellation,
and event-loop bridging. A concurrency-library SLP may define a Scheduler runner for applications
that deliberately want source-owned root scheduling; that API need not replace this entry contract.

## OpenSpec realization map

No handoff exists while this proposal is Draft. A future accepted direction would likely map to:

1. complete-entry external-park capability analysis;
2. root execution and storage lifecycle;
3. synchronous target wait/poll and final-outcome adaptation;
4. target compatibility diagnostics; and
5. direct/nested-only pay-for-use and explicit-child independence evidence.

## Revision and decision record

| Revision | Date | Change or decision |
| --- | --- | --- |
| 1 | 2026-08-22 | Split the repeated root-ownership fork from SLP-0001 after the author selected the smaller substrate scope. Preserved ordinary `run` as the desired API, isolated synchronous entry adaptation and storage as this Draft's thesis, and left asynchronous host integration out of scope. |
