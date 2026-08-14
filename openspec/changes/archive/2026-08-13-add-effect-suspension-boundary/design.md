## Context

See [proposal.md](proposal.md) for motivation. The current representation separates a hidden
construction-site Effect environment from a generated runner. `RunEffectValue` executes such an
environment; MIR normalization can replace eligible local single-use runs with `RunStaticEffect`.
Both LLVM and Wasm currently lower either operation to an ordinary target call. There is no
`musttail` or `return_call` premise to extend, and measured tail recursion fails at the same depth
as non-tail recursion.

The evaluator is already different: `executeMachine` drives generator-backed activations from a
heap array and enforces `maxCallDepth` from the number of source-logical activations. It therefore
does not need stack protection, but it does need suspension, allocation, tracing, and resource
accounting semantics that match compiled execution.

Four existing constraints shape the design:

- only the sealed `Intrinsic` namespace may expose compiler-known callable behavior;
- Effects carry exact typed failure and requirement rows;
- allocations are observable through the selected source-defined `Allocator`, with typed
  `OutOfMemory` and self-contained reclaim authority; and
- source cleanup is deterministic for structured exits and typed failure, but traps carry no
  unwind promise.

## Goals / Non-Goals

**Goals:**

- make explicitly suspended non-tail self- and mutual-Effect recursion stack safe on native and
  Wasm;
- keep the public abstraction source-defined and the continuation representation target-neutral;
- preserve exact ownership, failure propagation, service requirements, tracing, and `CallDepth`;
- make continuation allocation and exhaustion honest in the Effect type;
- demonstrate zero suspension overhead in closed non-suspending artifacts; and
- select the private native lowering by comparing direct emission with LLVM coroutine lowering
  behind the same target-neutral MIR contract.

**Non-Goals:**

- infer suspension or automatically transform recursion;
- make ordinary function calls, recursive traversal, or Drop hooks stack safe;
- add concurrency, scheduling fairness, interruption, cancellation, fibers, promises, or a public
  pending state;
- add a universal Effect interpreter or make every Effect use a continuation ABI;
- standardize an LLVM coroutine ABI as a Silk ABI or require other engines to emulate one; or
- solve the separate borrow-heavy Wasm `unreachable` observed in a deep `Box` walk.

## Decisions

### 1. `Effect.suspend` widens rows explicitly

The public source definition is designed as:

```silk
pub effect fn suspend<A, !E, ?R>(
  deferred: once Effect<A ! E ? R>
) -> A ! E | OutOfMemory ? R | &mut Allocator {
  return run Intrinsic.suspendEffect(move deferred)
}
```

A scalar non-tail cycle therefore states the cost in its own signature:

```silk
effect fn count(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  if value == 0 { return 0 }
  let next = run Effect.suspend(effect { return run count(value - 1) })
  return next + 1
}
```

Continuation state cannot be hidden or infallible: native and Wasm need storage proportional to
the number and layout of retained logical activations, and no finite machine can promise that
storage always exists. A hidden backend heap would violate allocator-service selection, erase
deterministic allocation failure, and create cleanup authority absent from the source contract.
A trap would make ordinary resource exhaustion unrecoverable and disagree with every other owned
allocation. Widening only the explicit boundary keeps the cost honest and preserves zero-cost
synchronous Effects.

Alternatives rejected:

- **Hidden infallible allocation:** violates the existing owned-allocation contract and cannot be
  tested deterministically.
- **Trap on exhaustion:** places expected exhaustion outside the typed channel.
- **Add allocation rows to every Effect:** destroys pay-for-use and overstates requirements.
- **Use a fixed stack or arena:** merely moves the hidden finite limit and still needs an exhaustion
  policy.

### 2. Admit one transfer-shaped intrinsic, not an abstraction-shaped runtime API

Add one safe generic intrinsic, represented here as `Intrinsic.suspendEffect`, whose logical
contract is the same widened Effect contract as the wrapper. It transfers a deferred Effect to the
active compiler-owned driver. It does not expose frame allocation, frame push/pop, resume labels,
step results, or pending values. `Effect.suspend` is ordinary source and no compiler phase branches
on `silk.effects` or its spelling.

The operation is safe because its compiler contract performs layout, ownership transfer,
allocation, and cleanup; callers establish no raw-memory invariant. The intrinsic inventory remains
the source of signature, target availability, and verification coverage.

Alternatives rejected:

- **Recognize `Effect.suspend` by name:** violates minimal compiler privilege.
- **Expose `Continuation<T>` plus management operations:** leaks backend representation and permits
  invalid ownership states.
- **Take a zero-argument function thunk:** Silk has no anonymous function literal able to capture
  the recursive variable; an Effect is already the lazy capture value.

### 3. Mark suspendability by reachability and split synchronous from suspendable entry

After instance discovery, compute a deterministic fixed point over the concrete call graph. A
runner is suspendable when it directly contains the suspension intrinsic or can call/apply/run a
suspendable instance. Generated Effect runners are classified by their exact Effect identity rather
than by a factory instance that merely constructs them. This fact is recorded in shared
target-aware MIR and participates in verification and encoding.

Suspension MIR is built in four ordered stages:

1. Monomorphic lowering assigns stable source-derived identities and execution classification to
   runners, explicit suspension origins, and calls that may relay suspension. `SuspendEffect`
   originates transfer; `RunSuspendableEffect` may either complete synchronously or relay transfer.
2. MIR normalization uses the concrete runner classification. It may keep the established direct
   forms for proven synchronous runners, but preserves materialization and the provisional
   suspendable control forms for suspendable or unknown runners.
3. Backward liveness runs over that post-normalization MIR, so it sees every specialized MIR local,
   including compiler-generated temporaries such as the saved left operand of
   `left + run suspended`. It classifies each live local as Copy, an exact borrowed dependency, or
   an affine transfer and derives refusal-rollback, resumed-success, and resumed-failure plans.
4. Lowering finalizes acyclic suspension regions and continuation descriptors from those facts.

A finalized suspension point has stable success and failure resume identities and a continuation
descriptor containing:

- the deferred runner identity, type arguments, captures, and provider arguments;
- the exact typed outcome and propagation mappings;
- locals live after the child, with logical types, planned layouts, and Copy/borrow/move access;
- success and failure resume regions; and
- releases and loan endings for every path.

The MIR descriptor does not prescribe whether a target stores a function pointer, table index, or
numeric resume tag. MIR regions remain a DAG; recursion exists only in the module call graph and the
runner's target-neutral resume relation.

Static Effect normalization may still fold construction into `RunStaticEffect` for a runner proven
synchronous. Suspendable or unknown candidates retain materialization and use the suspendable run
form. Outcome reification, entry closure, and provided generated runners use the same concrete
classification rather than bypassing the suspension-compatible execution boundary. The existing
`Options.suspension` shortcut must be replaced by the reachability fact rather than treating
suspension as a global compilation mode.

Alternatives rejected:

- **Annotate only the intrinsic call:** callers would still recursively enter suspendable callees
  with ordinary calls.
- **Make all runs suspendable execution boundaries:** violates the established synchronous artifact
  guarantee.
- **Encode backend frame fields directly in MIR:** couples the target-neutral contract to ABI
  choices and breaks deterministic cross-target planning.

### 4. Each stateful transfer relay owns one explicit allocation before the child starts

For each `RunSuspendableEffect` that observes transfer and needs distinct post-child resume state,
target-neutral planning produces a logical payload containing its live post-resume locals and
control identity. The selected backend derives one complete validated physical allocation request
containing its private control header plus that finalized post-normalization payload. A control-only
resume still requires a frame; a tail relay that can propagate the child and typed outcome unchanged
without post-child work may add none. The explicit suspension origin creates transfer but does not
allocate merely for originating it. A branch that completes synchronously or does not reach
`Effect.suspend` makes no request merely because its runner is suspendable. Successful storage is
initialized transactionally and then becomes a single affine continuation owner. Refusal of the
current request moves no current-activation value and creates no owner; any earlier unpublished
frames are rolled back before unchanged `OutOfMemory` returns, and the deferred body does not begin.

An explicit `SuspendEffect` origin first returns an unpublished transfer request; it does not start
the deferred child itself. As the request returns through `RunSuspendableEffect` callers, every
caller with live post-resume state allocates and initializes its continuation, prepending it to the
same unpublished chain. A caller whose child completes synchronously allocates nothing. The driver
publishes the whole chain and begins the deferred child only after all required frames are ready.
If any outer allocation or initialization fails, already-initialized unpublished frames roll back
in reverse order, remaining values stay owned by their current activations, and the unchanged typed
`OutOfMemory` returns without child execution.

After successful initialization, the allocation service call ends and its exclusive provider loan
is closed before the driver begins the deferred child. The continuation retains only its
self-contained reclaim authority. This is crucial when the child also needs `&mut Allocator`: it
must borrow the service afresh rather than conflict with an allocation borrow retained by its
parent. A provider selected for continuation storage must itself be proven non-suspendable; this
prevents recursively needing continuation storage in order to allocate continuation storage.

Frames are allocated individually rather than through a growable private vector. That directly uses
the existing allocation and reclaim model, gives one observable failure point per frame-producing
relay, and allows incremental cleanup without an additional hidden capacity policy. Multiple
callers relaying one explicit origin therefore make one request each when each needs resume state,
in inner-to-outer relay order. Physical header fields, sizes, and alignments may differ by engine,
provided the selected allocator sees every complete validated target request and no backend performs
a second hidden allocation.

This requirement is a feasibility gate for LLVM lowering, not an assumption that switched-resume
automatically satisfies it. Standard switched-resume normally creates its coroutine object at ramp
entry. The spike must therefore prove a one-to-one mapping between each coroutine object and one
frame-producing relay, in which ramp entry occurs only after transfer is observed, exposes the full
target size and alignment to the selected allocator, preserves typed refusal before moving current
values or starting the child, and introduces no second allocation. If it cannot, switched-resume is
rejected rather than weakening the public allocation contract.

Alternatives rejected:

- **Grow one hidden frame stack:** growth policy and spare capacity become observable allocation
  behavior with extra rollback complexity.
- **Keep the allocator provider borrowed in the frame:** conflicts with nested allocation and
  violates self-contained reclaim authority.
- **Permit a suspending allocator implementation:** creates an unbootstrappable recursive boundary.

### 5. The semantic protocol is iterative; native lowering is selected by a gated spike

`SuspendEffect` is the only form that may originate a fresh `Transfer(child, origin)`, and it never
produces a source `Complete`. `RunSuspendableEffect` invokes a potentially suspendable runner. If
the callee returns `Complete(outcome)`, the caller continues or propagates directly in its current
activation without allocating or entering a resume region. If the callee returns an existing
`Transfer`, the caller may only preserve its child and origin identities, transactionally prepend
the continuation required after that run, and relay it. Refusal while preparing that caller state
first rolls back the incoming unpublished chain, then becomes
`Complete(Failure(OutOfMemory))`; it never starts the deferred child.

The target-neutral protocol has two conceptual outcomes that never appear as Silk values:

- **Complete(outcome):** the current runner produced its typed success/failure outcome.
- **Transfer(unpublished chain, child):** an explicit suspension originated a deferred child and
  each relaying caller with live post-resume state prepended one initialized continuation.

On `Transfer`, the active ordinary call chain returns to the private driver without recursively
starting the child. The driver atomically publishes the fully initialized continuation chain and
invokes the child entry. On `Complete`, it either returns the final Effect outcome or pops one
frame, resumes it with the child outcome, destroys transferred payload state as dictated by the
resume path, and reclaims the frame. Refusal before publication rolls back the unpublished chain
and never starts the child. This conceptual loop performs no recursive driver-to-driver call and is
the cross-engine contract; its physical function ABI is not.

Direct WebAssembly realizes the protocol with private step entries, linear-memory frame addresses,
compiler-owned dispatch identities, and an iterative driver. Its exact header lane widths and
dispatch encoding remain target choices derived from target layout.

Native lowering is deliberately gated rather than preselected. Before production suspension MIR
exists, a hand-built frozen spike schema models the target-neutral descriptor fields from decision
3 without becoming a compiler or serialized-MIR API. The same schema is lowered through two
private LLVM strategies:

1. **Direct state machine:** private step entries and an explicit iterative driver, matching the
   original design.
2. **LLVM switched-resume:** `llvm.coro.*` structure intrinsics with Silk-controlled allocation,
   typed exhaustion, cleanup, and an explicit resume loop that does not require symmetric tail
   transfer.

The spike is pinned to LLVM 22.1.8 and the repository's native target and O0/O2 profiles. Its fixture
contains two reached suspension points, an untaken suspension branch, live affine state, success,
typed failure, refusal at every allocation ordinal, and a harness-only orderly private teardown
after each suspension checkpoint. The teardown asserts raw frame reclamation separately and does
not promise source Drop, cancellation, interruption, or a public pending handle. A depth sweep at
1, 1,000, and 100,000 records stack high-water evidence and the driver call skeleton.

Hard gates are evaluated before performance: one visible allocation per frame-producing relay and
none for the origin, a tail relay, synchronous completion, or the untaken branch; full size and
alignment through the selected allocator; unchanged typed
`OutOfMemory`; no child or owner after refusal; and the successful-path event order
`allocation-begin → allocation-end/loan-close → publish → child-start`, with the child successfully
reborrowing the same allocator. Success and typed-failure traces must match the direct reference's
source-cleanup order and unchanged failure payload before exactly-once frame reclaim. Refusal traces
must clean the current activation and all earlier frames in the specified order. Harness-only defect
teardown is a separate case: it reclaims raw private blocks and must report no source cleanup.

The remaining hard gates are bounded machine stack; source locations on ramp, resume, cleanup, and
destroy blocks plus a symbolized suspension checkpoint naming the originating Silk boundary; no
required tail calls, exceptions, or hidden allocation; and no coroutine machinery or ABI drift in
the synchronous control. Stack high-water is measured within one process from a no-inline probe's
volatile local address at every driver checkpoint. The 100,000-depth run may not extend the observed
address range by more than 4 KiB beyond the maximum range of the 1- and 1,000-depth runs, and
post-lowering call-graph/disassembly inspection must find no driver/resume/child recursive cycle.
Failure of the direct reference strategy blocks the design. A failing LLVM strategy is rejected.

For passing strategies, five warmups and thirty measured runs record medians and median absolute
deviations for O0/O2 compilation time and O2 resume time, plus exact allocated frame bytes per
modeled frame-producing relay and optimized linked code/data size. Timing deltas are normalized per
modeled relay where applicable. A timing difference is material when it is at least ten percent of the
direct median and its absolute value exceeds `2 × (MAD_direct + MAD_candidate)`. A frame difference
is material at the greater of ten percent or sixteen bytes; an artifact difference is material at
the greater of ten percent or 256 bytes.

Switched-resume is selected only when it passes every hard gate, has at least one material measured
advantage over direct lowering, and has no material regression on another metric. Parity selects
direct lowering because it avoids the extra LLVM-coroutine maintenance surface. Otherwise
returned-continuation is measured only when its buffer/continuation ABI can plausibly address the
failed allocation-layout/control gate or measured regression; it is selected only under the same
hard gates and advantage/no-regression rule. If neither LLVM strategy qualifies, direct lowering
wins.

The pinned spike selected direct lowering. Switched-resume passed every hard gate but showed no
material advantage and regressed O2 compilation plus allocator-visible frame size. The frame
regression triggered the conditional returned-continuation comparison; Retcon also passed every
hard gate but showed no material advantage and increased the selected allocation from 24 to 64
bytes per boundary. The machine-readable operands and generated decision table live in the native
suspension lowering characterization fixture. The rejected coroutine constructions remain
spike-local evidence and do not enter production compiler or LLVM surfaces.

LLVM async lowering is excluded on native-local grounds: its frontend-owned async-context ABI,
frontend allocation/lifetime burden, and intended tail-transfer protocol add coupling without
providing the switched-resume lifecycle and standard-lowering advantages being evaluated.

Rust is evidence for owning suspension liveness, borrow, layout, and destruction semantics before
backend selection. It is not evidence that LLVM switched-resume satisfies Silk's allocator
contract, that every suspension must allocate, or that recursive polling is stack safe.

The selected LLVM representation remains private and replaceable. LLVM explicitly does not
guarantee coroutine compatibility across releases, so the pinned toolchain and structural tests
must absorb that instability rather than MIR, source contracts, or serialized artifacts.
Direct synchronous runners retain the current outcome-return ABI under every candidate.

No accepted strategy may depend on a target-tail-call feature: non-tail state lives in frames and
the controlling loop is ordinary iteration. No exception, `setjmp`, JavaScript promise, or host
callback represents typed control flow.

Alternatives rejected:

- **Choose direct lowering without comparison:** duplicates LLVM frame-splitting machinery without
  first measuring whether the pinned backend can provide it within Silk's contracts.
- **Choose LLVM switched-resume from documentation alone:** leaves explicit typed allocation,
  affine cleanup, debugging, and pay-for-use assumptions untested in Silk's builder and pipeline.
- **Use returned-continuation first:** it resembles `Complete` / `Transfer`, but LLVM documents
  distinct buffer and abnormal-resume semantics; switched-resume is the standard lowering and
  lower-risk first measurement for this pinned toolchain.
- **Use LLVM async lowering:** couples native execution to a Swift-shaped async context and intended
  tail transfer while leaving allocation and lifetime policy with the frontend.

### 6. Ownership is finalized from specialized post-normalization MIR liveness

Source binding availability is not sufficient continuation liveness: it over-captures dead values,
cannot name compiler temporaries, and cannot decide whether a generic `T` is Copy before concrete
specialization. The continuation planner therefore runs after monomorphic lowering and
suspendability-aware normalization. Backward control-flow liveness computes the exact MIR locals
used on any resume successor. Copy locals are copied when needed; affine locals move exactly once
into initialized slots; borrows retain exact loan, access, and root dependencies, whose owners remain
live and immovable. A borrowed local and its affine root are represented as one dependency plus one
root transfer when both survive, never as duplicate owners.

The planner produces three distinct path families. Allocation refusal or initialization failure
uses a prefix rollback table: initialized affine slots release in reverse initialization order and
unmoved suffix values remain owned by their current activations. Resumed success consumes or
restores transferred values for the success continuation and preserves the later structured exits.
Resumed typed failure ends the required loans and executes the same inner-to-outer propagation
releases as the equivalent unsuspended path before reclaiming frame storage. A typed failure cannot
be returned to an outer handler until every exited continuation has completed its cleanup. Tests
record one owner per level and sweep every initialization prefix so missing, duplicate, or reordered
cleanup is visible.

Source traps and unrecoverable target defects preserve Silk's existing no-unwind contract: the
design does not claim that source Drop hooks run. An orderly driver teardown that still has control
may reclaim compiler-private raw frame blocks, but that bookkeeping is not reported as source
cleanup and must never execute or duplicate source Drop. This keeps defect handling from silently
creating stronger source semantics.

### 7. Evaluation reuses the activation machine and preserves logical `CallDepth`

The evaluator models the same suspension allocation and ownership events, but needs no second
physical continuation stack. Its existing heap `ActivationRecord` stack is the logical stack. A
suspension request retains the parent activation, pushes the deferred child, and resumes the parent
with the typed result exactly as an ordinary generated call does.

`maxCallDepth` continues to count active source-logical invocations. A parent retained across
suspension remains one unit; its child adds one. Compiler-generated driver, allocation adapter, and
resume helpers add zero logical units. Thus the default limit can block a program that is physically
stack safe, and raising it permits the same program without risking JavaScript stack overflow. The
blocked result's active path includes suspended parents and points at the attempted child boundary.

A separate continuation-count limit is not introduced: it would overlap `CallDepth`, while actual
storage exhaustion is already represented by `OutOfMemory`. Step accounting continues to count
executed MIR operations, including explicit suspension and allocation operations.

Alternatives rejected:

- **Exclude suspended parents from `CallDepth`:** permits unbounded heap continuations despite the
  existing definition of active invocation.
- **Count private helper calls:** makes evaluator limits depend on implementation decomposition.
- **Add a continuation limit now:** duplicates existing logical-depth and storage-exhaustion bounds.

### 8. Existing source combinators need no pending-aware branch

`map`, `flatMap`, `result`, `catch`, `retry`, and provision remain unchanged ordinary Silk control
flow. Suspendability propagates through their specialized concrete call graphs. If a protected or
selected Effect can suspend, its `run` retains suspension-aware control and the exact locals that
survive concrete normalization are classified as continuation payload only when that run relays
transfer. A synchronous `Complete` uses the ordinary current activation and allocates no frame.
Consequently `map` keeps its mapper and `flatMap` keeps the success callback state without either
knowing a private pending representation.

Conformance covers suspension before a mapper, suspension selected by `flatMap`, typed failure
through `result` and `catch`, and provision where provider loans and acquired owners cross only the
lexical boundaries already prescribed by their source definitions.

### 9. Pay-for-use is proven at four layers

Text scans are insufficient because nameless allocations or branches can remain. Tests use a
pinned closed synchronous corpus and assert:

1. **Reachability:** no concrete function is marked suspendable.
2. **MIR:** no suspendable run, suspension point, continuation descriptor, or driver entry exists;
   existing `RunStaticEffect` normalization verdicts and direct-call shape remain pinned.
3. **LLVM:** parsed bitcode/IR and linked symbols contain no private driver, frame allocation,
   resume dispatch, or complete-versus-transfer branch; the synchronous entry/call skeleton matches
   the pinned fixture.
4. **Wasm:** decoded sections/instructions/imports contain no driver function/table, continuation
   allocation path, resume dispatch, or complete-versus-transfer branch; the synchronous entry/call
   skeleton matches the pinned fixture.

Positive suspendable fixtures assert the inverse so the absence checks cannot pass because
inspection stopped recognizing the feature. Compiler-private symbol names and operation tags are
central constants consumed by emitters and tests, not loose regex vocabulary.

### 10. `Box` is characterization, not suspension acceptance

Add three fixtures with equivalent chain length and payload, each classifying exactly one recursive
phase on native release and Wasm:

- **build:** construct recursively through the Effect and consume the completed chain with an
  iterative `Box.into` teardown;
- **walk:** construct iteratively, traverse through the recursive borrowed walk, and then use the
  same iterative consuming teardown; and
- **drop:** construct iteratively and trigger ordinary recursive `Box.drop` cleanup without a
  recursive Effect build or walk.

The iterative teardown releases every allocation normally. It does not suppress cleanup, retain a
chain past process exit, or introduce a characterization-only leak.

The fixture records success, process signal, host exception, or Wasm trap plus the tested depth and
engine versions. It does not assert that `Effect.suspend` fixes any phase. A limiting ordinary walk,
Drop, or the borrow-heavy Wasm `unreachable` will be filed separately and remains outside this
design.

## Risks / Trade-offs

- **[One allocation per stateful relay is expensive]** → The feature is explicit, tail relays and
  synchronous completion allocate nothing, non-suspending code remains unchanged, and
  characterization measures frame size and allocation counts before considering a separate batching
  optimization.
- **[Suspendability spreads through generic combinators]** → Compute it after concrete instance
  discovery and pin deterministic fixed-point results for `map` and `flatMap` specializations.
- **[Frame liveness or rollback bugs duplicate affine values]** → Make post-normalization MIR-local
  ownership plans part of verified MIR, reject stale pre-normalization locals, and sweep every
  allocation/initialization failure ordinal with one tracked owner per level.
- **[Exclusive allocator access can conflict with the child]** → End the allocation call's provider
  loan before publishing the transfer; retain only reclaim authority, and reject suspending
  allocator implementations.
- **[Private ABI divergence between native and Wasm]** → Compare typed outcomes, allocation/release
  traces, frame logical layouts, and cleanup order against evaluation while allowing only physical
  header differences.
- **[LLVM coroutine lowering adds a second native maintenance surface]** → Keep it behind the same
  MIR contract, pin LLVM 22 transformation evidence, and select it only if the gated spike shows a
  material implementation or generated-code advantage over direct emission.
- **[The spike accidentally becomes a language ABI decision]** → Record only measured private
  backend evidence; prohibit LLVM coroutine handles, function shapes, and frame layouts from source,
  MIR, serialization, or cross-engine conformance.
- **[Trap cleanup is misunderstood as guaranteed]** → Keep source trap tests negative: no Drop trace
  is promised. Test private frame reclamation separately from source cleanup.
- **[Pay-for-use assertions become vocabulary scans]** → Inspect parsed MIR, LLVM, Wasm sections,
  instructions, symbols, imports, and a pinned synchronous call skeleton, with positive controls.
- **[Deep acceptance tests are slow]** → Keep million/one-hundred-thousand release tests in the
  release-candidate lane and use smaller structural tests in the ordinary suite.

## Migration Plan

1. Complete Box recursive-phase isolation and scalar exhaustion characterization without changing
   runtime behavior.
2. Run the hand-built native lowering spike, record its complete evidence package, and select the
   replaceable private native strategy.
3. Add the intrinsic catalog entry, public source wrapper, row typing, and concrete suspendability
   analysis.
4. Add stable provisional suspension MIR, suspendability-aware normalization, post-normalization
   MIR-local ownership planning, finalized continuation descriptors, and verifier-complete tests.
5. Add evaluator semantics and deterministic allocation-failure/`CallDepth` tests.
6. Implement the selected native realization.
7. Add the direct Wasm private runner and three-engine outcome and cleanup parity tests.
8. Strengthen pay-for-use inspection and enable deep release-candidate acceptance tests.
9. Sync these delta specs only after implementation passes repository checks and
   `pnpm release:candidate`.

Before the implementation merges, rollback is removal of the new source API, intrinsic, MIR forms,
and unreachable private runner code as one change. After release, the project remains pre-stable and
may remove the API rather than preserve a faulty ABI; no serialized continuation or public ABI is
promised.

## Open Questions

- The exact private intrinsic spelling may change during implementation as long as the catalog
  contains exactly one operation and no public contract depends on that spelling.
- The Wasm resume-dispatch encoding is a target-local choice to settle with its focused prototype;
  it does not change MIR, source rows, cleanup, or pay-for-use requirements.
- The native spike selects direct emission or LLVM switched-resume before native implementation.
  Returned-continuation is the conditional fallback described in decision 5. This is an explicit
  implementation gate with fixed criteria, not a deferred semantic question; every qualifying
  outcome preserves the same source and MIR contract.
