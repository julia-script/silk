## Context

See `proposal.md` for motivation and the delta specs for behavior. The current suspension pipeline
already has useful pieces: concrete suspendability classification, explicit `SuspendEffect` and
`RunSuspendableEffect` control, post-normalization liveness, deterministic layouts, iterative native
and Wasm drivers, and a heap-activation evaluator. The incorrect seam begins where each stateful
relay becomes a separately allocated continuation record selected through the source `Allocator`.

That seam currently spreads through `SuspensionMir`, `ContinuationLayout`,
`ContinuationTransaction`, `SuspensionOwnership`, MIR verification, evaluator allocation events,
both compiled backends, `Instances.continuationAllocatorViolations`, diagnostic `SEM0102`, the
intrinsic and standard-library signature, and many tests and examples. This is a green-field
correction: the obsolete path is deleted rather than retained behind compatibility behavior.

One concrete invocation has a statically bounded set of suspension states, but recursive execution
has dynamically many active invocations. The design therefore needs a fixed maximum frame layout
per specialized invocation and a dynamic compiler-owned execution stack. A source borrow that lives
across suspension also requires its referent's physical location to remain stable.

## Goals / Non-Goals

**Goals:**

- Preserve the existing reachability-based split between direct and suspendable execution.
- Represent each specialized suspendable invocation as one tagged frame whose storage is reused
  across its resume states.
- Give the iterative driver a private non-moving stack of active frames with fatal growth failure.
- Preserve typed outcomes, ownership, loans, providers, cleanup, deterministic logical depth, and
  target parity.
- Delete all source allocator selection, typed continuation OOM, allocator traces, rollback
  transactions, reclaim authority, and allocator-recursion diagnostics associated with suspension.
- Keep frame layout and execution-stack mechanics target-neutral through MIR and target-private
  below it.

**Non-Goals:**

- Preserve source or test compatibility with the widened suspension signature.
- Generalize this change into parking, scheduling, fibers, generators, async cleanup, or
  interruption.
- Select LLVM coroutine intrinsics or standardize a backend frame ABI.
- Make ordinary recursion, recursive `Drop`, or uncovered Effect recursion stack safe.
- Expose execution-stack sizing, allocation, or failure as a source service in this change.

## Decisions

### 1. Keep suspendability reachability; replace relay records with per-invocation states

Concrete specialization and call-graph reachability remain the gate for coroutine lowering. A
closed graph that cannot reach `SuspendEffect` keeps the current direct Effect representation and
backend call shape.

For each specialized suspendable runner, MIR planning produces one `CoroutineFrameDescriptor` in
concept, replacing the aggregate of separately owned continuation descriptors. It contains:

- a stable runner identity and private state discriminant;
- arguments, captures, and environment fields needed by the invocation;
- one state description for every suspension resume point;
- the exact post-normalization source and compiler-generated locals live in each state;
- state-specific ownership, loan, provider, success, failure, and cleanup plans; and
- one maximum logical layout capable of representing any state.

The maximum frame behaves like a tagged union: fields from mutually exclusive states may share
storage when layout and ownership permit, but each live value has one deterministic field identity
within its state. Reaching suspension finishes the parent state transition and returns transfer to
the driver. Resuming changes the discriminant and continues in the same frame. No transfer creates
a new record for the same invocation.

Alternative: minimally strip allocator fields from current per-relay records while retaining one
private allocation per relay. Rejected because it preserves the wrong ownership model, prevents
frame reuse across suspension points, and makes a compiler lowering accident the architecture.

### 2. Create an invocation frame when suspendable execution begins

When a suspendable Effect begins running through the private driver, its arguments and captures
enter one stable invocation frame before its body can establish a borrow that may cross suspension.
This avoids moving an already-borrowed local when a later branch first suspends. A path that
completes without reaching `Effect.suspend` still uses that invocation's frame; avoiding fields or
dispatch on such a path is an optimization, not a semantic guarantee.

Stored but unrun Effect values keep their existing owned capture representation. The execution frame
is created when the Effect is run, not when the Effect value is constructed. Dropping an unrun
Effect therefore retains the ordinary stored-Effect cleanup path.

Alternative: start every invocation directly and materialize a frame only at the first transfer.
Rejected as the baseline because relocating live referents and reconstructing internal loans at that
point makes stable borrowing substantially harder. A backend may perform equivalent escape
elision only when it proves address and cleanup equivalence.

### 3. Use a private non-moving segmented execution stack

The private driver owns a LIFO stack of active invocation frames. Frames have variable statically
known sizes because mutually recursive runners may have different descriptors. Pushing a deferred
child acquires space for its descriptor; completion or typed failure pops the child and supplies its
outcome to the parent state.

The baseline storage strategy is non-moving segmented growth:

- an active frame never changes address;
- growth acquires another private segment rather than relocating earlier frames;
- empty trailing segments may be released privately;
- segment headers, frame headers, growth policy, and physical allocation remain backend-private;
  and
- inability to acquire the next segment enters the existing target fatal-trap path without source
  cleanup or a typed outcome.

Native toolchain support may obtain private segments through its target implementation. Direct Wasm
reserves non-overlapping private linear-memory regions through backend memory-growth support.
Evaluation uses heap activation objects and its deterministic step/`CallDepth` limits; host memory
exhaustion remains fatal rather than a simulated `OutOfMemory` Effect. None of these mechanisms
consults or aliases a source `Allocator` provider.

Alternative: one growable contiguous arena. Rejected as the baseline because growth can relocate
frames and invalidate borrows; using offsets for every reference would impose a new reference ABI.

Alternative: one fixed global arena. Rejected because the arbitrary fixed capacity would become an
undocumented program limit and make deep acceptance tests target-configuration accidents.

### 4. Keep origin and relay control, but make both state transitions

`SuspendEffect` remains the sole explicit origin of a deferred-child transfer.
`RunSuspendableEffect` remains the compiler control form by which an ordinary caller can either
complete synchronously or relay an existing transfer. This distinction is useful for transforming
ordinary source combinators without exposing pending state.

The payload changes:

- an origin records its resume state in its invocation frame and identifies the deferred child;
- a stateful relay records its own resume state in its invocation frame and preserves the incoming
  child/origin/outcome identity;
- a relay with no post-child work may be reduced to a frame replacement or direct transfer when
  ownership and cleanup are equivalent; and
- there is no unpublished allocation chain, allocation ordinal, partial publication, or rollback
  transaction.

The driver receives a complete chain of logical state transitions by returning through suspendable
callers, then begins the deferred child iteratively. Each transition must be ownership-complete
before the child runs.

### 5. Replace continuation layout and transaction actors rather than adapting them

`ContinuationTransaction` exists to prepare allocator requests, handle refusal ordinals, publish a
chain, and reclaim partial prefixes. It has no role after this change and is deleted with its tests.

`ContinuationLayout` is replaced by a coroutine-frame planning actor (final actor name chosen during
implementation) that aggregates state layouts by specialized runner and computes one maximum
physical layout per target. Target-neutral MIR retains logical types and state fields; physical
native and Wasm headers remain backend-private.

`SuspensionMir` stops discovering allocator providers or attaching allocator arguments. It remains
responsible for finalizing origin/relay identities, associating them with invocation states, and
producing deterministic frame descriptors after normalization. `SuspensionOwnership` plans
state-specific moves, loans, and cleanup without adding allocator locals.

MIR verification is rewritten around state completeness and maximum-frame consistency. Allocation
refusal, prefix rollback, captured reclaim authority, and synchronous-path allocation rules are
deleted rather than translated.

### 6. Remove suspension allocation from types and diagnostics at the source boundary

The intrinsic callable contract and shipped wrapper become:

```silk
pub effect fn suspend<A, E, ?R>(
  deferred: once Effect<A ! E ? R>
) -> A ! E ? R
```

Elaboration and row composition receive no special subtraction or compatibility behavior; the
ordinary generic channels already express the complete contract. Existing sources, fixtures, and
Labs presets remove allocator provision and `OutOfMemory` recovery when those existed only for
suspension.

`Instances.continuationAllocatorViolations`, diagnostic `SEM0102`, its reason payload, catalog entry,
rendering, and tests are deleted. Allocator implementations may suspend under their ordinary Effect
contracts because they are no longer selected to bootstrap coroutine storage.

`OutOfMemory` and `Allocator` remain unchanged for actual source-owned allocation.

### 7. Preserve ownership by storing cross-suspension roots in stable frames

Every affine local live in a suspended state has exactly one frame field owner. Copy locals may be
reproduced according to their ordinary rules. Loans retain the same root, access, and lexical
dependency that source analysis established.

Because frames are non-moving while active, a referent stored in a frame may remain borrowed across
child execution without a source `Pin` type. Provider access follows the same rule: an exclusive
provider loan retained by the parent remains exclusive while the child runs. Dead loans and values
are omitted by post-normalization liveness.

Success and typed failure execute the state-specific cleanup plan and pop the completed frame once.
A fatal trap, including execution-stack growth failure, retains Silk's global no-unwind rule. The
private storage implementation may release process/runtime memory during termination, but it does
not report source `Drop` or manufacture a typed outcome.

### 8. Replace allocator traces with coroutine-state traces

Evaluation removes `ContinuationRequest` and `ContinuationRelease` events and all scripted
allocation-refusal machinery for suspension. It retains `SuspensionOrigin` and records deterministic
logical frame push, state transition, resume, and frame completion events sufficient to compare
ownership and cleanup across engines. Event names and payloads are compiler API data and are updated
atomically with all consumers; no aliases for the obsolete events remain.

A suspended parent remains an active source-logical invocation for `CallDepth`. Before pushing a
child frame, evaluation applies the existing next-invocation limit check. Driver/state helper work
does not add depth. This preserves deterministic blockage without pretending that suspension erased
logical recursion.

### 9. Preserve the direct state-machine backend strategy

The prior native spike selected Silk's direct iterative lowering over LLVM switched-resume and
returned-continuation. This change does not reopen that decision. Native and Wasm adapt their
existing private driver loops to push, run, transition, and pop invocation frames from the private
execution stack.

Backend acceptance proves:

- bounded machine/host stack for deep non-tail and mutual recursion;
- exact success, typed-failure, retained-owner, and borrow behavior;
- repeated suspension reuses one invocation frame;
- fatal private stack exhaustion does not enter Effect channels;
- native and Wasm artifacts contain no source allocator call on suspension paths; and
- non-suspending artifacts contain no coroutine frames, driver, stack helper, or pending branch.

## Risks / Trade-offs

- **[Private execution storage can still exhaust]** → Use one documented fatal trap class and never
  imply infinite recursion capacity or typed recovery.
- **[Segment growth can collide with target memory management]** → Reserve segments through each
  backend's private non-overlapping memory support and add mixed source-allocation/suspension stress
  tests on native and Wasm.
- **[Stable borrows can be broken by an optimization]** → Make address stability a verifier/backend
  invariant and require cross-suspension borrow tests before permitting frame elision or relocation.
- **[Aggregating states may increase individual frame size]** → Use tagged-union state liveness and
  deterministic field reuse; measure frame layouts without turning byte counts into correctness
  assertions.
- **[Large deletion surface can leave ghost allocator behavior]** → Add repository-wide structural
  searches and artifact tests for `SEM0102`, continuation allocator arguments, allocator trace
  events, transaction actors, and suspension-only row widening.
- **[Tests may accidentally keep handling real allocation failure]** → Remove only scaffolding whose
  sole source is suspension; retain `OutOfMemory` and providers for vectors, boxes, buffers, and
  other actual source allocations.

## Migration Plan

1. Pin red contract and structural tests for exact channels, frame reuse, no allocator selection,
   stable borrows, logical depth, target parity, and non-suspending artifact shape.
2. Change the intrinsic and standard-library signatures, then update source fixtures to expose every
   compiler dependency on the removed rows.
3. Replace MIR continuation descriptors/transactions with per-runner coroutine frame states and
   update ownership and verification.
4. Adapt evaluation, native, and Wasm drivers to private non-moving execution stacks; remove
   allocator traces, refusal paths, reclaim logic, and `SEM0102`.
5. Update stored-Effect, combinator, Labs, formatting, diagnostics, and acceptance suites; delete
   superseded code and fixtures.
6. Run the required full verification and SLP/OpenSpec/implementation audit, then update canonical
   specs and language/API documentation to implemented truth.

Rollback is source control reversion before archive. No compatibility mode, dual signature, or
runtime switch is retained in the completed change.
