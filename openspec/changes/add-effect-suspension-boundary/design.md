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
- make continuation allocation and exhaustion honest in the Effect type; and
- demonstrate zero suspension overhead in closed non-suspending artifacts.

**Non-Goals:**

- infer suspension or automatically transform recursion;
- make ordinary function calls, recursive traversal, or Drop hooks stack safe;
- add concurrency, scheduling fairness, interruption, cancellation, fibers, promises, or a public
  pending state;
- add a universal Effect interpreter or make every Effect use a continuation ABI; or
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
suspendable instance. This fact is recorded in shared target-aware MIR and participates in
verification and encoding.

Ordinary `RunEffectValue` and `RunStaticEffect` remain direct synchronous operations only when the
selected runner is proven non-suspendable. A run of a suspendable runner enters the private driver
through a distinct target-neutral MIR operation. A suspension point has a stable resume identity
and a continuation descriptor containing:

- the deferred runner identity, type arguments, captures, and provider arguments;
- the exact typed outcome and propagation mappings;
- locals live after the child, with logical types, planned layouts, and Copy/borrow/move access;
- success and failure resume regions; and
- releases and loan endings for every path.

The MIR descriptor does not prescribe whether a target stores a function pointer, table index, or
numeric resume tag. MIR regions remain a DAG; recursion exists only in the module call graph and the
runner's data-driven resume loop.

Static Effect normalization may still fold construction into `RunStaticEffect` for a runner proven
synchronous. Suspendable or unknown candidates retain materialization and use the suspendable run
form. The existing `Options.suspension` shortcut must be replaced by the reachability fact rather
than treating suspension as a global compilation mode.

Alternatives rejected:

- **Annotate only the intrinsic call:** callers would still recursively enter suspendable callees
  with ordinary calls.
- **Make all runs driver calls:** violates the established synchronous artifact guarantee.
- **Encode backend frame fields directly in MIR:** couples the target-neutral contract to ABI
  choices and breaks deterministic cross-target planning.

### 4. Continuations are single-block owned records allocated before the child starts

At each suspension point, layout planning produces one record containing a private header plus all
live post-resume values. One validated allocation is requested through `&mut Allocator`; successful
storage is initialized transactionally and then becomes a single affine continuation owner. If
allocation fails, no value has moved, the deferred body does not begin, and ordinary typed-failure
cleanup runs in the current activation.

After successful initialization, the allocation service call ends and its exclusive provider loan
is closed before the driver begins the deferred child. The continuation retains only its
self-contained reclaim authority. This is crucial when the child also needs `&mut Allocator`: it
must borrow the service afresh rather than conflict with an allocation borrow retained by its
parent. A provider selected for continuation storage must itself be proven non-suspendable; this
prevents recursively needing continuation storage in order to allocate continuation storage.

Frames are allocated individually rather than through a growable private vector. That directly
uses the existing allocation and reclaim model, gives one failure point per logical boundary, and
allows incremental cleanup without an additional hidden capacity policy. Backends may choose a
target-private header before the compiler-planned payload, provided the allocator sees the complete
validated request and the layout meets the requested alignment.

Alternatives rejected:

- **Grow one hidden frame stack:** growth policy and spare capacity become observable allocation
  behavior with extra rollback complexity.
- **Keep the allocator provider borrowed in the frame:** conflicts with nested allocation and
  violates self-contained reclaim authority.
- **Permit a suspending allocator implementation:** creates an unbootstrappable recursive boundary.

### 5. The private protocol is iterative and target-specific

The target-neutral protocol has two conceptual outcomes that never appear as Silk values:

- **Complete(outcome):** the current runner produced its typed success/failure outcome.
- **Transfer(frame, child):** the current runner saved post-resume state and asks the driver to run
  a deferred child.

The driver owns the linked continuation top. On `Transfer`, it installs the already initialized
frame and invokes the child entry. On `Complete`, it either returns the final Effect outcome or
pops one frame, resumes it with the child outcome, destroys transferred payload state as dictated
by the resume path, and reclaims the frame. The loop performs no recursive driver-to-driver call.

For LLVM, reachable suspendable runners receive private step entry functions with an erased driver
context and target-planned payload pointers; direct synchronous runners retain the current outcome
return ABI. For Wasm, reachable suspendable runners receive equivalent private step entries and a
driver loop using private linear-memory frame addresses and compiler-owned dispatch identities.
The exact header lane widths and dispatch encoding are backend choices derived from target layout,
not source or MIR types.

No target-tail-call feature is used: non-tail state lives in frames, and the driver loop itself is
ordinary iteration. No exception, `setjmp`, JavaScript promise, or host callback represents typed
control flow.

### 6. Ownership plans frame initialization, resumption, and cleanup

Liveness at the suspension point determines the frame payload. Copy locals are copied when needed;
affine locals move exactly once into initialized slots; borrows move as dependencies whose roots
remain live and immovable. Frame initialization is ordered so every prefix has a compiler-known
rollback cleanup. The continuation becomes published to the driver only after the full payload is
initialized.

On child success or typed failure, the chosen resume path consumes the child payload once, runs the
same MIR releases and loan endings the unsuspended path would run, and then drops remaining frame
fields in lexical order before reclaiming storage. A typed failure cannot be returned to an outer
handler until every exited continuation has completed its cleanup. Tests record one owner per
level so missing, duplicate, or reordered cleanup is visible.

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
selected Effect can suspend, its `run` becomes a suspendable MIR execution boundary and the
combinator's locals after that run become ordinary continuation payload. Consequently `map` keeps
its mapper and `flatMap` keeps the success callback state without either knowing a private pending
representation.

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
Drop, or the borrow-heavy Wasm `unreachable` is filed separately and remains outside this design.

## Risks / Trade-offs

- **[One allocation per suspension is expensive]** → The feature is explicit, non-suspending code
  remains unchanged, and characterization will measure frame size and allocation counts before
  considering a separate batching optimization.
- **[Suspendability spreads through generic combinators]** → Compute it after concrete instance
  discovery and pin deterministic fixed-point results for `map` and `flatMap` specializations.
- **[Frame liveness or rollback bugs duplicate affine values]** → Make ownership-produced live sets
  part of verified MIR and sweep every allocation/initialization failure ordinal with one tracked
  owner per level.
- **[Exclusive allocator access can conflict with the child]** → End the allocation call's provider
  loan before publishing the transfer; retain only reclaim authority, and reject suspending
  allocator implementations.
- **[Private ABI divergence between native and Wasm]** → Compare typed outcomes, allocation/release
  traces, frame logical layouts, and cleanup order against evaluation while allowing only physical
  header differences.
- **[Trap cleanup is misunderstood as guaranteed]** → Keep source trap tests negative: no Drop trace
  is promised. Test private frame reclamation separately from source cleanup.
- **[Pay-for-use assertions become vocabulary scans]** → Inspect parsed MIR, LLVM, Wasm sections,
  instructions, symbols, imports, and a pinned synchronous call skeleton, with positive controls.
- **[Deep acceptance tests are slow]** → Keep million/one-hundred-thousand release tests in the
  release-candidate lane and use smaller structural tests in the ordinary suite.

## Migration Plan

1. Land Box phase-isolation and scalar exhaustion fixtures without changing runtime behavior.
2. Add the intrinsic catalog entry, public source wrapper, row typing, suspendability analysis, and
   target-neutral MIR behind verifier-complete tests.
3. Add evaluator semantics and deterministic allocation-failure/`CallDepth` tests.
4. Add native then Wasm private runners with three-engine outcome and cleanup parity tests.
5. Strengthen pay-for-use inspection and enable deep release-candidate acceptance tests.
6. Sync these delta specs only after implementation passes repository checks and
   `pnpm release:candidate`.

Before the implementation merges, rollback is removal of the new source API, intrinsic, MIR forms,
and unreachable private runner code as one change. After release, the project remains pre-stable and
may remove the API rather than preserve a faulty ABI; no serialized continuation or public ABI is
promised.

## Open Questions

- The exact private intrinsic spelling may change during implementation as long as the catalog
  contains exactly one operation and no public contract depends on that spelling.
- The LLVM private header field order and Wasm resume-dispatch encoding are target-local choices to
  settle with focused prototypes; they do not change MIR, source rows, cleanup, or pay-for-use
  requirements.
