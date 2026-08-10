# Concurrency and parallelism direction

Status: exploratory direction, not an accepted language design or scheduled milestone.

Last reviewed: 2026-08-10.

## Purpose

Silk should eventually preserve the synchronization properties that make Effect useful—structured
execution, bounded queues, backpressure, interruption, and deterministic cleanup—without imposing a
managed-runtime cost on programs that do not use them. This note records the direction and the
constraints future discovery must respect. It does not select syntax, promise async I/O, or move
concurrency into the bootstrap milestone.

The existing [Effect pattern corpus](effect-pattern-corpus.md) remains the scenario inventory. This
note narrows its concurrency and stream observations into a cost model suitable for a low-level,
ahead-of-time-compiled language with explicit ownership and allocation.

## Distinctions to preserve

- **Incremental demand** lets a consumer request the next item only after processing the previous
  one. A sequential pull stream can therefore bound its working set without fibers.
- **Concurrency** lets multiple logical executions make progress by suspending and resuming. It can
  run deterministically on one operating-system thread.
- **Parallelism** executes work simultaneously on multiple threads or cores and additionally needs
  thread-safe ownership, synchronization, and scheduling.
- **Logical blocking** means an Effect does not complete until an operation succeeds or fails. It
  does not require blocking the operating-system thread; a future runtime may park the current
  fiber instead.

Concurrency does not imply parallelism, and neither is required to define useful sequential Stream
and Sink composition.

## Directional cost contract

Silk should remain cost-transparent and pay-for-use rather than claiming that every abstraction is
literally free.

| Reachable program behavior | Directional lowering | Runtime cost permitted |
| --- | --- | --- |
| Ordinary functions | Direct calls | None |
| Non-suspending Effects | Direct structured control with typed exits and explicit provider arguments | No scheduler, fiber allocation, atomics, or mandatory runtime context |
| Suspension in the current execution | Generated resumable state and continuation storage | Only state live across suspension plus park/wake dispatch |
| Forked or independently progressing Effects | Scheduled child fiber state | Ready-queue and task-storage costs |
| Parallel execution | Explicitly selected multi-thread execution | Threads, synchronization, atomics, and applicable transfer restrictions |

Consequences:

- A program that cannot reach suspension or concurrency should link no scheduler and allocate no
  fiber objects.
- Static Effect and Stream composition should monomorphize and fuse where semantics permit. Dynamic
  erasure, boxing, or indirect dispatch must be explicit or independently justified by reachable
  behavior.
- Provider specialization should become direct arguments and calls when statically known. A
  replaceable provider does not by itself justify a runtime dictionary lookup at every operation.
- Suspension-frame sizes, hidden allocations, queue capacity, peak live tasks, and runtime symbols
  should be inspectable and testable compiler evidence.
- Fiber-safe code is not automatically thread-safe. A future parallel execution mode may impose
  additional transfer and sharing requirements without burdening a single-thread executor.

## Effect execution and fibers

An Effect is a lazy description; a fiber is one running execution. The directional semantic model
is that an entry Effect runs in a root fiber, sequentially composed Effects continue in the current
fiber, and only fork or another concurrent operator creates an independently scheduled child fiber.
This model does not require a heap object for the root fiber or for every nested Effect. The compiler
may erase the fiber context whenever no reachable behavior can suspend, fork, interrupt, or observe
it.

Runtime suspension is distinct from the accepted `Effect.suspend` operation used for deferred
construction and stack-safe trampolining. A runtime suspension parks an unfinished execution,
retains its continuation and live state, and permits another ready execution to advance. A future
compiler may lower suspension-capable Effects to stackless resumable state machines while compiling
non-suspending Effects as ordinary direct code.

The current source-defined Effect library deliberately observes only completed typed outcomes
through `Effect.result`. That operation and propagating `run` are the suspension-compatible seams:
future lowering may park and later resume the current execution before either produces its value,
without exposing a pending constructor to `map`, `flatMap`, or `catch`. Requirements remain erased
type rows plus compiler-selected provider arguments; no runtime environment record is part of the
public model. The synchronous cost spike measures this actual library/core boundary before any
complete-or-suspended representation or optimizer is selected.

The smallest plausible runtime seam is correspondingly narrow: schedule or resume a frame, park it
on a wait condition, wake that condition, track structured children, and interrupt with deterministic
cleanup. Queue, Deferred, Semaphore, TaskGroup, and concurrent Stream behavior should be library
modules over that seam rather than reasons to interpret every Effect through a universal runtime.

## Ownership and allocation

Concurrency must not introduce an implicit global heap. Future designs should consider explicit
executor or allocator ownership, bounded preallocated task storage, and inspectable task/frame
sizes. Forking necessarily retains state somewhere; the cost must be attributable to the operation
and execution adapter that requested it.

Retaining synchronization modules own values while those values are queued or parked:

- a successful offer transfers an owned value from producer to queue;
- a take transfers it from queue to consumer;
- a parked offer must have one explicit owner for its retained value;
- interruption, shutdown, dropping, and sliding strategies need exactly one cleanup or return path
  for queued, rejected, displaced, pending, and in-flight values; and
- structured child execution must not outlive borrowed providers or storage it can still access.

Effect's garbage-collected implementation is behavioral evidence for synchronization, not evidence
for Silk's ownership representation.

## Stream, Sink, and standard I/O

The eventual dependency direction should be:

```text
Stream and Sink composition
          |
          v
standard-I/O adapters
          |
          v
primitive effectful read/write operations
          |
          v
native, WebAssembly, and test hosts
```

A sequential pull Stream may provide genuine demand-driven backpressure before concurrency exists.
It should not claim independent producers, concurrent mapping, merging, asynchronous sources, or
queue-backed behavior until the execution model can park and resume work honestly.

The current `StandardStreams.writeAll` contract is a suitable primitive seam because its Effect
completes only after the complete byte view is written or a typed failure occurs. Its bootstrap host
provider and `@v1` WebAssembly import complete synchronously, but later adapters may park a fiber
without changing that logical source contract. Future stdout and stderr should be Sink adapters;
future stdin should be a Stream adapter over a primitive effectful read operation. Standard I/O
must not define the general Stream abstraction.

The current source-level `bool` encoding of stdout versus stderr is bootstrap-shaped and should be
revisited before it becomes a lasting public representation. A nominal output destination or
output handle would leave room for Stream and Sink adapters without exposing backend encoding.

## Possible capability ladder

This is an order of semantic dependency, not a roadmap commitment:

1. Sequential pull Stream and Sink composition with bounded working-set evidence.
2. A deterministic single-thread structured-execution kernel with fork, join, park, wake,
   interruption, and child cleanup.
3. Bounded Queue as the ownership and backpressure acceptance case.
4. Concurrent Stream operators built above the same queue and execution semantics.
5. Host I/O wakeups and timers when a concrete program requires them.
6. Opt-in parallel execution with an explicit thread-safety and transfer model.

## Revisit triggers

Promote this direction into focused design work only when a real program needs one of the following:

- incremental input substantially larger than its permitted working set;
- an independent producer and consumer whose progress must be coordinated;
- a bounded compiler pipeline that exchanges owned chunks;
- host I/O that should not block other logical work;
- cancellation or racing with observable cleanup requirements; or
- parallel compiler work that demonstrates value beyond deterministic sequential execution.

## Open questions

- Is suspension only a compiler-internal effect summary, or does any public contract need to promise
  non-suspension?
- Is the scheduler an execution adapter, a requirement, or a private facility introduced only by a
  closed entry point?
- How are task frames allocated, bounded, and inspected on embedded, native, and WebAssembly
  targets?
- What structured lifetime permits child fibers to access providers or borrows without admitting
  unrestricted stored borrows?
- What do dropping and sliding queue offers return when the rejected or displaced element is an
  affine owner?
- Which fairness and determinism guarantees should the evaluator, a single-thread native executor,
  and later parallel executors share?
- Which host ABI can wake a suspended WebAssembly execution without making the bootstrap `@v1`
  standard-stream import permanent?

## Premature commitments to avoid

- A universal heap-allocated Effect interpreter for all programs.
- Linking a scheduler into programs that cannot suspend or fork.
- Treating every nested Effect as a new fiber.
- Equating concurrency with threads or fiber safety with thread safety.
- Modeling stdout as the Stream abstraction rather than as an adapter target.
- Exposing synchronous host completion as a permanent language guarantee.
- Adding a non-blocking `WouldBlock` substitute under a future backpressured `offer` interface.
- Choosing general async cleanup, shared ownership, or stored-borrow machinery before a workload
  demonstrates the smallest necessary model.
