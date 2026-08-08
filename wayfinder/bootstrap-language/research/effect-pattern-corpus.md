# Effect pattern corpus for Silk execution-model review

Research date: 2026-08-07.

Primary current target: Effect `4.0.0-beta.102`, which this repository pins, from the official
`Effect-TS/effect` source at commit
[`de2a9a6`](https://github.com/Effect-TS/effect/tree/de2a9a69099993087e57c64df58537c765ac0224),
plus `@effect/platform-node` `4.0.0-beta.103` at
[`dff2544`](https://github.com/Effect-TS/effect/tree/dff25449dfc927f2cce912c329f343cfb5365f88).
The public Effect v3 documentation and the final v3 source line remain historical and explanatory
evidence because v4 is still beta and its detailed first-party reference is primarily in source
documentation. Earlier entries retain the documented v3 teaching syntax where the semantic pattern
is unchanged; the multiple-scope and service-lifetime expansion below is verified directly against
the installed v4 beta versions. This is a semantic corpus, not a migration reference and not a
proposal for Silk syntax.

## Purpose and limits

The question is not whether Silk can imitate Effect's API. It is whether the behaviors that make
Effect useful survive Silk's constraints: ahead-of-time compilation, explicit ownership, lexical
borrows, synchronous typed-infallible Drop, explicit allocation, finite monomorphization, no
garbage collector, and no mandatory heap-shaped interpreter object.

Each pattern therefore records:

- the user problem;
- the canonical TypeScript Effect expression;
- the semantic property that makes the expression useful; and
- a language-neutral scenario that can later become a Silk conformance example.

The first half avoids explicit resource acquisition. The second half applies resource and allocation
pressure. Effect itself runs on garbage-collected JavaScript and does **not** provide evidence for a
raw-memory ownership model. Its resource APIs do provide primary evidence for scope, finalization,
interruption, concurrency, and composition behavior. Every memory-allocation scenario below is
deliberately labeled as a **Silk stress test**, not as behavior inherited from Effect.

## Decision outcome

The 2026-08-07 design and adversarial review used this corpus as pressure, not as a template. The
accepted bootstrap direction is:

- `effect {}` is the primitive lazy imperative boundary, `effect fn` is its function sugar, and the
  public language abstraction is named `Effect`, not Flow;
- capture modes derive shared, exclusive-repeatable, or consuming execution, and retry is legal only
  for repeatable effects;
- affine owners plus synchronous infallible `Drop` handle bootstrap cleanup on structured exits and
  typed failure; traps do not promise unwinding;
- allocator results are self-contained owners that do not retain the allocator provider;
- named scopes, dynamic finalizer registries, hidden resource-dependency sets, `depends on`, arena-
  backed escaping values, concurrency, cancellation, and asynchronous cleanup are deferred; and
- a future arena remains ordinary standard-library code and receives no compiler privilege.

Entries below that explore named scopes or provider-dependent values remain intentionally preserved
as rejected/deferred stress cases. They are useful tests for a future design round, not normative
bootstrap requirements. The authoritative decisions are Wayfinder issues 01, 03, 07, and 08.

## Taxonomy at a glance

| Family | Patterns | Primary semantic pressure |
| --- | --- | --- |
| Program descriptions | laziness, suspension, imperative sequencing, boundary lifting | construction versus execution |
| Typed outcomes | expected failure, selective recovery, defects, accumulation, fallback | failure ownership and propagation |
| Requirements | services, provision, construction graphs, memoization | open versus closed programs |
| Time | retry, repetition, timeout | repeated execution and cancellation |
| Concurrency | bounded parallelism, racing, fibers, signals, queues, semaphores | interruption and structured lifetime |
| Retained work | caches, request batching, request caching | identity, sharing, retention, invalidation |
| Incremental work | streams and resourceful streams | backpressure and bounded working sets |
| Operations | logging, tracing, metrics, test clock | observation without semantic distortion |
| Resources | brackets, scopes, child lifetimes, early close, finalizer order, rollback | deterministic cleanup and region pressure |
| Services | stateless capabilities, stateful providers, construction and result scopes | distinct service and operation lifetimes |
| Explicit allocation | scratch buffers, owned results, retries, races, streams, caches | affine values and scope escape |

## Part I: common patterns without explicit allocation

### 1. Construct now, execute later

- **User problem:** describe work, compose or transform it, and choose when it actually interacts
  with the world.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.sync(() => externalAction())
  // externalAction has not run yet
  Effect.runPromise(program)
  ```

- **Semantic property:** an `Effect<A, E, R>` is an immutable, lazy program description. Constructing
  it performs no modeled interaction; a runtime later interprets it.
- **Primary source:** [The Effect Type](https://effect.website/docs/v3/getting-started/the-effect-type),
  [Creating Effects](https://effect.website/docs/v3/getting-started/creating-effects).
- **Silk test scenario:** construct a computation that increments an observable counter, prove the
  counter is unchanged before execution, then execute the value twice and record whether each run
  enters the body.

### 2. Delay effect construction itself

- **User problem:** avoid eager recursion or rerun construction-time logic on every execution.
- **Canonical Effect expression:**

  ```ts
  const recursive = (n: number): Effect.Effect<number> =>
    n < 2
      ? Effect.succeed(1)
      : Effect.zipWith(
          Effect.suspend(() => recursive(n - 1)),
          Effect.suspend(() => recursive(n - 2)),
          (a, b) => a + b
        )
  ```

- **Semantic property:** `Effect.suspend` defers evaluation of the thunk that creates the next
  effect. This is a second boundary beyond merely delaying the effect's interactions.
- **Primary source:** [Creating Effects: Suspended Effects](https://effect.website/docs/v3/getting-started/creating-effects).
- **Silk test scenario:** compare a recursively constructed computation that exhausts the host call
  stack with one whose recursive nodes are constructed during execution.

### 3. Imperative-looking sequential composition

- **User problem:** express branches, loops, local bindings, and early failure without manually
  nesting combinators.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.gen(function* () {
    const input = yield* readInput
    if (input.length === 0) return yield* Effect.fail(new EmptyInput())
    for (const item of input) yield* process(item)
    return input.length
  })
  ```

- **Semantic property:** the generator body is the lazy effect program. Ordinary control flow runs
  as that program executes, and yielding a failure aborts the remaining body.
- **Primary source:** [Using Generators](https://effect.website/docs/v3/getting-started/using-generators).
- **Silk test scenario:** put visible work before and after a conditional failure and inside a loop;
  verify construction is inert, execution order is lexical, and work after failure is skipped.

### 3a. Direct style and pipelines preserve one execution model

- **User problem:** choose the clearest surface form for a workflow without changing when it runs or
  what it owns, fails with, or requires.
- **Canonical Effect expression:**

  ```ts
  const direct = Effect.gen(function* () {
    const user = yield* loadUser(id)
    return yield* loadOrders(user.id)
  })

  const piped = loadUser(id).pipe(Effect.flatMap((user) => loadOrders(user.id)))
  ```

- **Semantic property:** generator direct style and combinator pipelines are alternative composition
  surfaces over the same lazy execution model; both compose success, failure, and requirements.
- **Primary source:** [Using Generators](https://effect.website/docs/v3/getting-started/using-generators),
  [Building Pipelines](https://effect.website/docs/v3/getting-started/building-pipelines).
- **Silk test scenario:** express the same workflow as a lazy block and as a pipeline, then compare
  body-entry timing, capture modes, residual failure row, requirement row, cleanup, and result.

### 4. Lift synchronous and asynchronous foreign boundaries

- **User problem:** turn throwing, promise-returning, or callback APIs into one composable execution
  model.
- **Canonical Effect expression:**

  ```ts
  const parse = Effect.try({
    try: () => JSON.parse(text),
    catch: (cause) => new ParseError({ cause })
  })

  const fetchUser = Effect.tryPromise({
    try: () => fetch(url),
    catch: (cause) => new TransportError({ cause })
  })
  ```

- **Semantic property:** foreign exceptions and rejections are captured at an explicit boundary and
  translated into the typed error channel; the boundary remains lazy.
- **Primary source:** [Creating Effects](https://effect.website/docs/v3/getting-started/creating-effects).
- **Silk test scenario:** wrap a host call that may return, fail recoverably, or violate an unsafe
  contract; confirm only the deliberately translated cases join the typed failure channel.

### 5. Expected failures as contract members

- **User problem:** know from a function's type which recoverable failures callers must consider.
- **Canonical Effect expression:**

  ```ts
  class NotFound extends Data.TaggedError("NotFound")<{ readonly id: string }> {}
  const lookup: Effect.Effect<User, NotFound> = Effect.fail(new NotFound({ id }))
  ```

- **Semantic property:** expected failures are values in `Effect`'s error parameter rather than
  invisible exceptions.
- **Primary source:** [Expected Errors](https://effect.website/docs/v3/error-management/expected-errors),
  [Creating Effects](https://effect.website/docs/v3/getting-started/creating-effects).
- **Silk test scenario:** originate one nominal failure, propagate it through two calls, and require
  the outer contract to retain the exact member until handled.

### 6. Selective recovery and residual errors

- **User problem:** recover one known failure without erasing unrelated failures.
- **Canonical Effect expression:**

  ```ts
  const recovered = program.pipe(
    Effect.catchTag("NotFound", (error) => loadDefault(error.id))
  )
  ```

- **Semantic property:** recovery is selected by the error's nominal tag. The handled member is
  removed while errors introduced by the handler and unmatched source errors remain.
- **Primary source:** [Expected Errors](https://effect.website/docs/v3/error-management/expected-errors),
  [Error Channel Operations](https://effect.website/docs/v3/error-management/error-channel-operations).
- **Silk test scenario:** protect a computation that can fail with `A | B`; handle only `A` with a
  handler that can fail with `C`; verify the resulting contract is `B | C`.

### 7. Expected failure, defect, and interruption remain distinct

- **User problem:** recover domain failures without accidentally treating programmer bugs or
  cancellation as ordinary business alternatives.
- **Canonical Effect expression:**

  ```ts
  const expected = Effect.fail(new InvalidInput())
  const defect = Effect.dieMessage("impossible state")
  const cancelled = Effect.interrupt
  const inspected = Effect.exit(program)
  ```

- **Semantic property:** Effect's `Cause` model distinguishes typed failures, defects, and
  interruption. Ordinary error recovery does not automatically absorb the latter two.
- **Primary source:** [Unexpected Errors](https://effect.website/docs/v3/error-management/unexpected-errors),
  [Cause](https://effect.website/docs/v3/data-types/cause).
- **Silk test scenario:** run the same handler around a typed failure, a trap, and cancellation;
  verify only the declared failure is recovered and cleanup policy remains explicit for the others.

### 8. Accumulate independent validation errors

- **User problem:** report all independent bad fields rather than stopping at the first one.
- **Canonical Effect expression:**

  ```ts
  const validated = Effect.all(
    { name: validateName(input.name), age: validateAge(input.age) },
    { mode: "validate" }
  )
  ```

- **Semantic property:** failure accumulation is an explicit combinator policy, not a hidden change
  to the default fail-fast error channel.
- **Primary source:** [Error Accumulation](https://effect.website/docs/v3/error-management/error-accumulation).
- **Silk test scenario:** validate three independent values, collect every validation failure into
  ordinary owned data, then compare with a sequential fail-fast composition.

### 9. Fallback without losing the original abstraction

- **User problem:** try a preferred operation and use an alternative only if it fails.
- **Canonical Effect expression:**

  ```ts
  const resilient = primary.pipe(Effect.orElse(() => secondary))
  ```

- **Semantic property:** fallback is lazy; the second program is not executed when the first
  succeeds, and its error/requirement contract composes into the result.
- **Primary source:** [Fallback](https://effect.website/docs/v3/error-management/fallback).
- **Silk test scenario:** make each branch mutate a separate counter; prove the fallback is neither
  constructed nor executed prematurely and that its requirements appear in the outer contract.

### 10. Requirements are part of the program type

- **User problem:** write business logic against capabilities while keeping dependencies visible
  and replaceable.
- **Canonical Effect expression:**

  ```ts
  class ClockService extends Context.Tag("ClockService")<
    ClockService,
    { readonly now: Effect.Effect<number> }
  >() {}

  const program = Effect.gen(function* () {
    const clock = yield* ClockService
    return yield* clock.now
  })
  ```

- **Semantic property:** the `Requirements` parameter records services still needed for execution.
  Reading a service adds that requirement without global lookup being hidden from the type.
- **Primary source:** [Managing Services](https://effect.website/docs/v3/requirements-management/services).
- **Silk test scenario:** compose two computations needing distinct capabilities and verify their
  open contract is the normalized union of both requirements.

### 11. Provision specializes an open program

- **User problem:** close over a concrete implementation at an application edge or substitute a
  deterministic test implementation.
- **Canonical Effect expression:**

  ```ts
  const runnable = program.pipe(
    Effect.provideService(ClockService, { now: Effect.succeed(123) })
  )
  ```

- **Semantic property:** provision removes a satisfied requirement and produces a new program; the
  original program remains reusable and open.
- **Primary source:** [Managing Services](https://effect.website/docs/v3/requirements-management/services).
- **Silk test scenario:** specialize the same open computation with two implementations and prove
  each specialized value executes against its own provider.

### 12. Separate service construction from service use

- **User problem:** construct a service from other services without leaking its implementation
  dependencies into every operation.
- **Canonical Effect expression:**

  ```ts
  const DatabaseLive = Layer.effect(
    Database,
    Effect.gen(function* () {
      const config = yield* Config
      const logger = yield* Logger
      return makeDatabase(config, logger)
    })
  )
  ```

- **Semantic property:** a `Layer<Out, E, In>` models an effectful dependency graph. Construction
  requirements and failures are distinct from the resulting service interface.
- **Primary source:** [Managing Layers](https://effect.website/docs/v3/requirements-management/layers).
- **Silk test scenario:** construct an owned provider using two temporary capabilities; after
  construction, operations expose only the provider's public contract, not its construction graph.

### 13. Share one constructed dependency intentionally

- **User problem:** prevent duplicate initialization when several services depend on the same
  expensive component.
- **Canonical Effect expression:**

  ```ts
  const graph = Layer.merge(
    Layer.provide(BLive, ALive),
    Layer.provide(CLive, ALive)
  )
  ```

- **Semantic property:** globally provided layers are memoized by identity; `ALive` is constructed
  once and shared, while `Layer.fresh(ALive)` deliberately requests separate instances.
- **Primary source:** [Layer Memoization](https://effect.website/docs/v3/requirements-management/layer-memoization).
- **Silk test scenario:** build a diamond dependency graph and count provider acquisitions and
  releases for shared and deliberately fresh variants.

### 14. Retry according to a value-level policy

- **User problem:** rerun transiently failing work with explicit delay, limit, and stopping rules.
- **Canonical Effect expression:**

  ```ts
  const policy = Schedule.exponential("100 millis").pipe(
    Schedule.intersect(Schedule.recurs(5))
  )
  const resilient = operation.pipe(Effect.retry(policy))
  ```

- **Semantic property:** retry re-executes the lazy operation only after failure, and the schedule is
  composable data describing recurrence.
- **Primary source:** [Retrying](https://effect.website/docs/v3/error-management/retrying),
  [Schedule Combinators](https://effect.website/docs/v3/scheduling/schedule-combinators).
- **Silk test scenario:** fail twice then succeed; observe three separate body entries and verify
  state created inside one attempt is not silently reused by the next.

### 15. Repeat successful work on a schedule

- **User problem:** poll, refresh, or run maintenance work after successful completions.
- **Canonical Effect expression:**

  ```ts
  const poller = poll.pipe(Effect.repeat(Schedule.fixed("10 seconds")))
  ```

- **Semantic property:** repetition is distinct from retry: it is driven by successful outcomes and
  creates a long-lived interruptible workflow.
- **Primary source:** [Repetition](https://effect.website/docs/v3/scheduling/repetition).
- **Silk test scenario:** repeat an operation until a success predicate is met, then cancel an
  infinite variant and verify no further iteration begins.

### 16. Bound waiting time with typed timeout

- **User problem:** stop waiting for work that exceeds a deadline and handle timeout as a declared
  outcome.
- **Canonical Effect expression:**

  ```ts
  const bounded = task.pipe(Effect.timeout("1 second"))
  const optional = task.pipe(Effect.timeoutOption("1 second"))
  ```

- **Semantic property:** timeout races the work with time, interrupts interruptible work, and can
  expose timeout either in the error channel or as ordinary result data.
- **Primary source:** [Timing Out](https://effect.website/docs/v3/error-management/timing-out).
- **Silk test scenario:** time out work before and after its first visible action; distinguish
  cancellation from typed domain failure and verify the chosen result encoding.

### 17. Traverse with bounded or unbounded concurrency

- **User problem:** process independent inputs in parallel without hand-writing task lifecycle and
  failure propagation.
- **Canonical Effect expression:**

  ```ts
  const results = Effect.forEach(inputs, process, { concurrency: 8 })
  ```

- **Semantic property:** concurrency is a policy on composition. The result preserves input order,
  and failure interrupts still-running siblings under the documented fail-fast mode.
- **Primary source:** [Basic Concurrency](https://effect.website/docs/v3/concurrency/basic-concurrency).
- **Silk test scenario:** run more jobs than the limit, record peak active jobs, then fail one and
  verify the remaining jobs stop according to the selected failure policy.

### 18. Race alternatives and cancel losers

- **User problem:** accept the first successful replica or strategy and stop spending resources on
  slower alternatives.
- **Canonical Effect expression:**

  ```ts
  const fastest = Effect.race(primary, replica)
  ```

- **Semantic property:** competitors run concurrently; the first success wins and losers are
  interrupted. If all fail, their failure cause is preserved according to the race operator.
- **Primary source:** [Basic Concurrency: Racing](https://effect.website/docs/v3/concurrency/basic-concurrency).
- **Silk test scenario:** race two delayed computations, attach finalizers to both, and prove the
  loser receives interruption before the winner is returned.

### 19. Fork, join, await, and interrupt explicitly

- **User problem:** start concurrent work, retain a typed handle, and later choose how to observe or
  cancel it.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.gen(function* () {
    const fiber = yield* Effect.fork(task)
    return yield* Fiber.join(fiber)
  })
  ```

- **Semantic property:** an effect is a lazy program; a fiber is its running execution. A fiber has
  an exit, identity, local state, and interruption lifecycle, but no remaining service requirements.
- **Primary source:** [Fibers](https://effect.website/docs/v3/concurrency/fibers).
- **Silk test scenario:** provide a requirement, fork the now-runnable computation, and verify the
  execution handle cannot escape the lifetime of state required by its running body.

### 20. Coordinate once with a deferred result

- **User problem:** let one task wait until another task publishes exactly one success or failure.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.gen(function* () {
    const ready = yield* Deferred.make<Value, LoadError>()
    yield* Effect.fork(
      load.pipe(
        Effect.matchEffect({
          onFailure: (error) => Deferred.fail(ready, error),
          onSuccess: (value) => Deferred.succeed(ready, value)
        })
      )
    )
    return yield* Deferred.await(ready)
  })
  ```

- **Semantic property:** `Deferred` is a single-assignment synchronization point. Await suspends a
  fiber without blocking an OS thread, and completion is idempotent.
- **Primary source:** [Deferred](https://effect.website/docs/v3/concurrency/deferred).
- **Silk test scenario:** race two producers completing one cell, verify exactly one wins, and prove
  the consumer observes the winning owned result once under Silk's ownership rules.

### 21. Apply backpressure with a bounded queue

- **User problem:** connect producers and consumers without allowing the producer to retain an
  unbounded backlog.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.gen(function* () {
    const queue = yield* Queue.bounded<Item>(64)
    yield* Queue.offer(queue, item)
    return yield* Queue.take(queue)
  })
  ```

- **Semantic property:** offering to a full bounded queue and taking from an empty queue suspend;
  shutdown interrupts suspended operations. Dropping and sliding queues make alternative overload
  policies explicit.
- **Primary source:** [Queue](https://effect.website/docs/v3/concurrency/queue).
- **Silk test scenario:** fill a fixed-capacity channel with owned values, suspend the next producer,
  consume one value, then close the channel and account for every queued and suspended value.

### 22. Limit access with automatically released permits

- **User problem:** constrain concurrent access to a scarce service even when tasks fail or are
  cancelled.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.gen(function* () {
    const semaphore = yield* Effect.makeSemaphore(3)
    yield* semaphore.withPermits(1)(criticalTask)
  })
  ```

- **Semantic property:** permits are acquired around an effect and guaranteed to be released after
  success, failure, or interruption.
- **Primary source:** [Semaphore](https://effect.website/docs/v3/concurrency/semaphore).
- **Silk test scenario:** cancel a task while it holds a permit, then prove a waiting task can
  acquire that permit and the count never exceeds the configured bound.

### 23. Memoize effectful work and make retention policy visible

- **User problem:** avoid duplicate work while choosing whether a result lives once, for a TTL, or
  until explicit invalidation.
- **Canonical Effect expression:**

  ```ts
  const cached = yield* Effect.cachedWithTTL(expensive, "30 seconds")
  const value1 = yield* cached
  const value2 = yield* cached
  ```

- **Semantic property:** computation remains lazy until the first evaluation; later evaluations
  share the stored result until expiry or invalidation.
- **Primary source:** [Caching Effects](https://effect.website/docs/v3/caching/caching-effects).
- **Silk test scenario:** execute a memoized computation concurrently, prove one computation feeds
  all waiters, invalidate it, and prove the next request creates a fresh result.

### 24. Deduplicate concurrent keyed work

- **User problem:** prevent a thundering herd when many fibers request the same absent value.
- **Canonical Effect expression:**

  ```ts
  const cache = yield* Cache.make({
    capacity: 100,
    timeToLive: "1 minute",
    lookup
  })
  const values = yield* Effect.all(
    [cache.get(key), cache.get(key), cache.get(key)],
    { concurrency: "unbounded" }
  )
  ```

- **Semantic property:** one lookup serves concurrent callers. Capacity, TTL, failure caching,
  interruption behavior, and invalidation are explicit parts of the cache abstraction.
- **Primary source:** [Cache](https://effect.website/docs/v3/caching/cache).
- **Silk test scenario:** request one missing key from several tasks, cancel one waiter, and verify
  the shared computation and retained value have a well-defined owner and lifetime.

### 25. Batch requests without changing calling code

- **User problem:** preserve individually typed queries while letting the runtime coalesce compatible
  requests into fewer backend operations.
- **Canonical Effect expression:**

  ```ts
  const resolver = RequestResolver.makeBatched(resolveUsers)
  const getUser = (id: number) =>
    Effect.request(GetUser({ id }), resolver)

  const users = Effect.forEach(ids, getUser, { batching: true })
  ```

- **Semantic property:** request descriptions retain sequential-versus-parallel structure, carry
  typed success and failure, and are fulfilled by a resolver that can inspect a whole batch.
- **Primary source:** [Batching](https://effect.website/docs/v3/batching),
  [RequestResolver source](https://github.com/Effect-TS/effect/blob/269516851b24916d72771f8a554b88722e3732e7/packages/effect/src/RequestResolver.ts).
- **Silk test scenario:** issue repeated and distinct keyed requests through nested compositions;
  verify batching preserves dependency order, deduplicates only equal requests, and completes every
  owned request exactly once.

### 26. Model zero-or-more values with backpressure

- **User problem:** process finite or infinite sequences without first materializing the whole
  sequence.
- **Canonical Effect expression:**

  ```ts
  const result = source.pipe(
    Stream.mapEffect(transform, { concurrency: 4 }),
    Stream.runCollect
  )
  ```

- **Semantic property:** `Stream<A, E, R>` extends the effect contract to zero or more values and
  composes incremental production, failure, requirements, and consumption.
- **Primary source:** [Stream Introduction](https://effect.website/docs/v3/stream/introduction),
  [Stream Operations](https://effect.website/docs/v3/stream/operations).
- **Silk test scenario:** consume a large sequence through a bounded transform while measuring the
  maximum number of in-flight elements and stopping early after a prefix.

### 27. Attach observability without changing the business contract

- **User problem:** understand execution, failures, and latency without manually threading telemetry
  objects through every function.
- **Canonical Effect expression:**

  ```ts
  const observed = operation.pipe(
    Effect.withSpan("compile-module"),
    Effect.tap((result) => Effect.log("compiled", result.id))
  )
  ```

- **Semantic property:** spans nest with effect composition, failures annotate span status, logs can
  become span events, and instrumentation does not change `Effect<A, E, R>`.
- **Primary source:** [Tracing](https://effect.website/docs/v3/observability/tracing),
  [Logging](https://effect.website/docs/v3/observability/logging),
  [Metrics](https://effect.website/docs/v3/observability/metrics).
- **Silk test scenario:** instrument a successful, failed, and cancelled execution; verify the same
  success/failure/requirement contract and correlate every nested event to the executing task.

### 28. Test time-driven programs deterministically

- **User problem:** test retries, timeouts, and polling without sleeping in real time.
- **Canonical Effect expression:**

  ```ts
  const fiber = yield* Effect.fork(task.pipe(Effect.timeout("1 minute")))
  yield* TestClock.adjust("1 minute")
  const exit = yield* Fiber.await(fiber)
  ```

- **Semantic property:** clock access is a replaceable service; advancing test time wakes scheduled
  fibers deterministically.
- **Primary source:** [TestClock](https://effect.website/docs/v3/testing/testclock).
- **Silk test scenario:** run a retrying computation under a deterministic clock, advance exactly to
  each deadline, and assert attempt count, cancellation, and cleanup without wall-clock delay.

## Part II: resource and allocation pressure

### 29. Bracket one resource around one use

- **User problem:** release a resource after use regardless of success, typed failure, defect, or
  interruption.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.acquireUseRelease(acquire, use, release)
  ```

- **Semantic property:** acquisition, use, and release form one unbreakable lifecycle. Release runs
  after the use phase completes on every exit path.
- **Primary source:** [Resource Management](https://effect.website/docs/v3/resource-management/introduction),
  [Effect source](https://github.com/Effect-TS/effect/blob/269516851b24916d72771f8a554b88722e3732e7/packages/effect/src/Effect.ts).
- **Silk stress test:** allocate a temporary buffer, use it to parse one input, and prove its owner
  is cleaned exactly once after success, typed failure, early return, and cancellation.

### 30. Compose several resources in one scope

- **User problem:** acquire resources in helper functions and let a larger workflow choose their
  common lifetime.
- **Canonical Effect expression:**

  ```ts
  const file = Effect.acquireRelease(openFile, closeFile)

  const program = Effect.scoped(
    Effect.gen(function* () {
      const input = yield* file
      const output = yield* file
      return yield* copy(input, output)
    })
  )
  ```

- **Semantic property:** `acquireRelease` returns a program requiring `Scope`; `Effect.scoped`
  supplies the lifetime boundary. Resource-producing helpers remain composable without choosing
  where the final outer scope closes.
- **Primary source:** [Scope: Defining Resources](https://effect.website/docs/v3/resource-management/scope).
- **Silk stress test:** let two helper calls allocate from the current named scope and verify neither
  helper closes the scope prematurely.

### 31. Release nested resources in reverse acquisition order

- **User problem:** tear down dependent resources safely: child before parent, file before
  connection, view before backing storage.
- **Canonical Effect expression:**

  ```ts
  const scoped = Effect.scoped(
    Effect.gen(function* () {
      yield* resourceA
      yield* resourceB
    })
  )
  ```

- **Semantic property:** scope finalizers execute last-in, first-out.
- **Primary source:** [Scope](https://effect.website/docs/v3/resource-management/scope).
- **Silk stress test:** allocate backing storage, then an owned index that depends on it; assert the
  index cleanup occurs before backing storage reclamation on every exit.

### 32. Register cleanup after dynamic acquisition

- **User problem:** associate cleanup with a value whose acquisition occurs conditionally or inside
  a loop.
- **Canonical Effect expression:**

  ```ts
  const program = Effect.gen(function* () {
    const value = yield* acquire
    yield* Effect.addFinalizer((exit) => release(value, exit))
    return value
  })
  ```

- **Semantic property:** finalizers can be registered dynamically in the current scope and receive
  its exit; they are run even on interruption.
- **Primary source:** [Scope: addFinalizer](https://effect.website/docs/v3/resource-management/scope).
- **Silk stress test:** grow a vector through several allocations and register cleanup as each owned
  block becomes live; fail halfway and account for each registered block in LIFO order.

### 33. Roll back a partially completed acquisition sequence

- **User problem:** build a multi-step resource where later acquisition may fail, leaving earlier
  external state to undo.
- **Canonical Effect expression:**

  ```ts
  const a = Effect.acquireRelease(acquireA, rollbackAOnFailure)
  const b = Effect.acquireRelease(acquireB, rollbackBOnFailure)

  const transaction = Effect.scoped(
    Effect.gen(function* () {
      const av = yield* a
      const bv = yield* b(av)
      return yield* commit(av, bv)
    })
  )
  ```

- **Semantic property:** each successful acquisition installs its own release action immediately;
  when a later step fails, already acquired resources unwind in reverse order.
- **Primary source:** [Scope: Sequencing Operations](https://effect.website/docs/v3/resource-management/scope).
- **Silk stress test:** reserve capacity, allocate a new block, then fail while moving elements;
  prove the old vector remains valid or the whole operation rolls back without leaking either block.

### 34. Make a resource-owning service scoped

- **User problem:** initialize a long-lived service once, share it across operations, and close it at
  the application or request boundary.
- **Canonical Effect expression:**

  ```ts
  class Database extends Effect.Service<Database>()("Database", {
    scoped: Effect.gen(function* () {
      const connection = yield* Effect.acquireRelease(connect, disconnect)
      return { query: (sql: string) => query(connection, sql) }
    })
  }) {}
  ```

- **Semantic property:** the service layer owns acquisition and release; operations see an already
  constructed service and do not repeat or leak construction requirements.
- **Primary source:** [Managing Layers: Lifecycle Control](https://effect.website/docs/v3/requirements-management/layers).
- **Silk stress test:** construct an allocator-backed compiler service once, call it several times,
  and close its storage only when the providing scope ends.

### 35. Memoize acquisition across a dependency diamond

- **User problem:** ensure two dependent services share the same owned pool or arena rather than
  each allocating a duplicate.
- **Canonical Effect expression:**

  ```ts
  const graph = Layer.merge(
    Layer.provide(ParserLive, ArenaLive),
    Layer.provide(AnalyzerLive, ArenaLive)
  )
  ```

- **Semantic property:** layer identity controls memoized construction; explicit freshness opts out.
  A scoped layer's finalizer belongs to the memoization scope.
- **Primary source:** [Layer Memoization](https://effect.website/docs/v3/requirements-management/layer-memoization).
- **Silk stress test:** provide one arena to two services, prove one acquisition and one release, then
  repeat with explicitly fresh arenas and prove independent lifetimes.

### 36. Clean each failed retry attempt before the next

- **User problem:** retry an operation that acquires temporary resources without retaining failed
  attempts until all retries finish.
- **Canonical Effect expression:**

  ```ts
  const attempt = Effect.acquireUseRelease(acquire, use, release)
  const retried = attempt.pipe(Effect.retry({ times: 3 }))
  ```

- **Semantic property:** the bracket is inside the retried effect, so each execution owns a complete
  acquire/use/release lifecycle before retry begins another execution.
- **Primary source:** [Resource Management](https://effect.website/docs/v3/resource-management/introduction),
  [Retrying](https://effect.website/docs/v3/error-management/retrying).
- **Silk stress test:** allocate scratch memory per attempt, fail twice, then succeed; verify peak
  live allocation equals one attempt rather than three.

### 37. Clean race losers before returning the winner

- **User problem:** race resourceful alternatives without leaking resources held by the cancelled
  branch.
- **Canonical Effect expression:**

  ```ts
  const fastest = Effect.race(
    Effect.acquireUseRelease(acquireA, useA, releaseA),
    Effect.acquireUseRelease(acquireB, useB, releaseB)
  )
  ```

- **Semantic property:** the losing branch is interrupted, interruption runs finalizers, and
  `Fiber.interrupt` ordinarily backpressures until termination.
- **Primary source:** [Basic Concurrency: Racing](https://effect.website/docs/v3/concurrency/basic-concurrency),
  [Fibers: Interruption](https://effect.website/docs/v3/concurrency/fibers).
- **Silk stress test:** let both branches allocate affine buffers; after the race, return the winning
  owned result while proving the loser's buffer was reclaimed and the winning result did not borrow
  from the loser's scope.

### 38. Time out resourceful work without abandoning cleanup

- **User problem:** return a timeout promptly while a task may be holding memory, handles, or locks.
- **Canonical Effect expression:**

  ```ts
  const bounded = Effect.acquireUseRelease(acquire, use, release).pipe(
    Effect.timeout("1 second")
  )
  ```

- **Semantic property:** timeout uses interruption for interruptible work; finalizers still run. An
  uninterruptible region delays timeout observation unless deliberately disconnected.
- **Primary source:** [Timing Out](https://effect.website/docs/v3/error-management/timing-out),
  [Resource Management](https://effect.website/docs/v3/resource-management/introduction).
- **Silk stress test:** time out while allocating, mutating, and releasing; identify the smallest
  uninterruptible acquisition/commit regions needed to avoid exposing partially initialized state.

### 39. Cancel a foreign async operation with its own cleanup

- **User problem:** adapt an external async API that allocates hidden resources and supplies a
  cancellation hook.
- **Canonical Effect expression:**

  ```ts
  const operation = Effect.async<Value, ForeignError>((resume, signal) => {
    const handle = startForeignOperation(resume)
    signal.addEventListener("abort", () => handle.cancel())
    return Effect.sync(() => handle.dispose())
  })
  ```

- **Semantic property:** `Effect.async` accepts an interruption cleanup effect and an `AbortSignal`;
  only the first resume wins.
- **Primary source:** [Creating Effects: From a Callback](https://effect.website/docs/v3/getting-started/creating-effects).
- **Silk stress test:** wrap a backend callback that owns an output buffer until completion; race
  completion with cancellation and prove exactly one path assumes responsibility for the buffer.

### 40. Bound parallel allocation with permits

- **User problem:** process many items concurrently while respecting a byte, connection, or workspace
  budget.
- **Canonical Effect expression:**

  ```ts
  const semaphore = yield* Effect.makeSemaphore(totalPermits)
  const run = (weight: number, task: Effect.Effect<Result, Error>) =>
    semaphore.withPermits(weight)(task)
  ```

- **Semantic property:** weighted permits bracket execution and are released on success, failure, or
  interruption.
- **Primary source:** [Semaphore](https://effect.website/docs/v3/concurrency/semaphore).
- **Silk stress test:** assign each task permits proportional to requested bytes, cancel tasks at
  several phases, and prove live reserved bytes never exceed the configured budget.

### 41. Stream through one resource lifetime

- **User problem:** keep a file, socket, decoder, or arena open exactly while its incremental stream
  is consumed.
- **Canonical Effect expression:**

  ```ts
  const stream = Stream.acquireRelease(openFile, (file) => file.close).pipe(
    Stream.flatMap((file) => readChunks(file))
  )
  ```

- **Semantic property:** resource acquisition occurs when stream consumption begins, and release
  occurs when consumption succeeds, fails, or stops early.
- **Primary source:** [Resourceful Streams](https://effect.website/docs/v3/stream/resourceful-streams).
- **Silk stress test:** read chunks into a reusable scratch buffer, stop after a prefix, and prove the
  stream closes the handle while no emitted owned value aliases reclaimed scratch storage.

### 42. Keep streaming allocation proportional to the working set

- **User problem:** transform a large or infinite input without retaining all decoded values or all
  temporary buffers.
- **Canonical Effect expression:**

  ```ts
  const output = source.pipe(
    Stream.mapEffect(transform, { concurrency: 4 }),
    Stream.runForEach(writeOutput)
  )
  ```

- **Semantic property:** stream demand and concurrency bound the number of elements being processed;
  interruption and early termination stop upstream work.
- **Primary source:** [Stream Introduction](https://effect.website/docs/v3/stream/introduction),
  [Stream Operations: concurrent effectful mapping](https://effect.website/docs/v3/stream/operations),
  [Consuming Streams](https://effect.website/docs/v3/stream/consuming-streams).
- **Silk stress test:** transform an input much larger than memory, record peak live buffers, and
  require that peak to follow chunk size and concurrency rather than total input size.

### 43. Account for owned values retained by queues

- **User problem:** know what happens to enqueued affine values when consumers stop or the queue is
  shut down.
- **Canonical Effect expression:**

  ```ts
  const queue = yield* Queue.bounded<OwnedItem>(capacity)
  yield* Queue.offer(queue, item)
  yield* Queue.shutdown(queue)
  ```

- **Semantic property:** queue capacity and overload strategy define retention; shutdown interrupts
  waiters and empties the queue.
- **Primary source:** [Queue](https://effect.website/docs/v3/concurrency/queue).
- **Silk stress test:** close bounded, dropping, and sliding channels containing owned values; prove
  dropped, displaced, queued, in-flight, and returned values each have one cleanup path.

### 44. Account for values retained by caches

- **User problem:** cache owned results without leaking evicted values or returning references to
  reclaimed storage.
- **Canonical Effect expression:**

  ```ts
  const cache = yield* Cache.make({ capacity, timeToLive, lookup })
  const value = yield* cache.get(key)
  yield* cache.invalidate(key)
  ```

- **Semantic property:** capacity, TTL, refresh, invalidation, concurrent lookup sharing, and
  interruption define when values enter and leave retention.
- **Primary source:** [Cache](https://effect.website/docs/v3/caching/cache).
- **Silk stress test:** decide whether a lookup returns a borrow, clone, shared owner, or moved value;
  then test eviction while callers still use a result. This deliberately probes a problem hidden by
  JavaScript garbage collection.

### 45. Return an owned result while reclaiming temporary allocation

- **User problem:** use an arena or scratch allocator internally but return a value that remains valid
  after the operation's local scope closes.
- **Canonical Effect analogue:**

  ```ts
  const result = Effect.acquireUseRelease(
    acquireWorkspace,
    (workspace) => computeOwnedResult(workspace),
    releaseWorkspace
  )
  ```

- **Semantic property:** the returned success value outlives the use phase, so it must not depend on
  a resource released by the bracket. Effect relies on ordinary JavaScript reachability here; Silk
  must prove the stronger ownership fact.
- **Primary source:** [Resource Management](https://effect.website/docs/v3/resource-management/introduction).
- **Silk stress test:** build a result using temporary arena nodes, then either move independent
  owned storage out or copy into an ancestor scope; reject a result containing a borrow into the
  closing workspace.

### 46. Define repeatability from captured ownership, not from a separate flag

- **User problem:** know whether a lazy computation can run repeatedly when it closes over affine,
  mutable, borrowed, or allocated state.
- **Canonical Effect contrast:**

  ```ts
  const repeated = Effect.suspend(() => operationUsingCapturedState)
  yield* repeated
  yield* repeated
  ```

- **Semantic property:** Effect values are normally reusable descriptions because JavaScript values
  are garbage-collected and freely shareable. Effect documentation warns that side effects or scoped
  captures may need `suspend` to recreate construction per execution. That does not answer affine
  capture semantics for Silk.
- **Primary source:** [Creating Effects: Suspended Effects](https://effect.website/docs/v3/getting-started/creating-effects),
  [The Effect Type](https://effect.website/docs/v3/getting-started/the-effect-type).
- **Silk stress test:** construct four lazy values capturing respectively a copyable scalar, shared
  borrow, exclusive borrow, and owned consumable buffer; derive permitted execution access and
  repeatability from those captures.

## Part III: multiple scopes and service lifetimes in Effect v4 beta

This section uses the API shipped by this repository's pinned `effect@4.0.0-beta.102` and
`@effect/platform-node@4.0.0-beta.103`. Statements under **Effect v4 behavior** are supported by the
linked first-party source documentation. Statements under **Silk inference** deliberately go beyond
what garbage-collected JavaScript and Effect's types prove.

### 47. Keep sibling resource sets on independently closeable child scopes

- **User problem:** one workflow owns several resource groups with different useful lifetimes, while
  one enclosing request or allocator still imposes their common maximum lifetime.
- **Canonical Effect v4 expression:**

  ```ts
  const program = Effect.scopedWith((requestScope) =>
    Effect.gen(function*() {
      const parseScope = yield* Scope.fork(requestScope)
      const cacheScope = yield* Scope.fork(requestScope)

      const parser = yield* parserResource.pipe(Scope.provide(parseScope))
      const cache = yield* cacheResource.pipe(Scope.provide(cacheScope))
      // parseScope and cacheScope can close at different times.
    })
  )
  ```

- **Effect v4 behavior:** `Scope.fork(parent)` creates a closeable child registered with its parent.
  Closing the child detaches it; closing the parent closes every still-open child with the parent's
  exit. `Scope.provide` selects the concrete scope used by an effect requiring the `Scope` service.
- **Primary source:** [Effect v4 beta `Scope.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Scope.ts),
  [Effect v4 beta `Effect.scopedWith`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Effect.ts).
- **Silk inference (deferred after review):** the JavaScript variable names above are not type-level
  region names. Effect's requirement row contains the one `Scope` service identity, not
  `ParseScope` and `CacheScope` as distinct lifetime parameters. Supporting values whose legal
  escape depends on sibling resource regions would require a general static validity model;
  bootstrap instead excludes that case rather than introducing named scope identities.
- **Silk stress test:** fork `parse` and `emit` scopes from an allocator-bounded request scope, close
  `parse` after producing an independently owned HIR, keep `emit` alive longer, and prove that
  neither child nor any value allocated from it can outlive the allocator's parent lifetime.

### 48. Drop a resource earlier than its allocator's maximum lifetime

- **User problem:** an allocator or arena may remain valid for a whole request, but a file, lock,
  temporary index, or object destructor should run immediately after its last logical use.
- **Canonical Effect v4 expression:**

  ```ts
  const temporaryScope = yield* Scope.fork(allocatorScope)
  const temporary = yield* temporaryResource.pipe(Scope.provide(temporaryScope))
  yield* use(temporary)
  yield* Scope.close(temporaryScope, Exit.void)
  // allocatorScope remains open for unrelated allocations.
  ```

- **Effect v4 behavior:** closing the child runs its registered finalizers immediately without
  closing the parent. An alternative for a single lexical use is `Effect.acquireUseRelease`, whose
  release runs directly after the `use` phase instead of waiting for an ambient scope.
- **Primary source:** [Effect v4 beta `Scope.close` and `Scope.fork`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Scope.ts),
  [Effect v4 beta resource APIs](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Effect.ts).
- **Silk inference:** Effect proves deterministic execution of the registered cleanup effect, not
  deterministic reclamation of the JavaScript object's memory. Silk should distinguish **logical
  drop** (run the destructor and make the value unusable) from **storage reclamation** (which an
  arena may defer until its maximum lifetime ends). Early drop is still valuable even when it cannot
  return the individual bytes to the allocator.
- **Silk stress test:** allocate three objects from one arena, drop the middle object's external
  handle and owned children early, continue using the other two, then reclaim all backing pages only
  when the arena closes. Reject any later use of the dropped object.

### 49. Choose a longer-lived owner during acquisition; do not pretend to transfer afterward

- **User problem:** a helper has a short local scope, but one acquired resource is intentionally
  owned by a caller-selected longer-lived scope.
- **Canonical Effect v4 expression:**

  ```ts
  const helper = (ownerScope: Scope.Scope) =>
    Effect.scoped(
      resource.pipe(Scope.provide(ownerScope))
    )
  ```

- **Effect v4 behavior:** `Scope.provide` routes a scope-requiring acquisition to an explicit scope;
  `Layer.buildWithScope` performs the analogous operation for a layer. In Effect v3 the closely
  corresponding scope combinator was named `Scope.extend`; v4's change to `Scope.provide` makes the
  service-provision operation more explicit. This is an API simplification, not evidence of a new
  region-transfer guarantee.
- **Primary source:** [Effect v4 beta `Scope.provide`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Scope.ts),
  [Effect v4 beta `Layer.buildWithScope`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Layer.ts),
  [Effect v3.22.1 `Scope.extend`](https://github.com/Effect-TS/effect/blob/417e0faa80e471d77fc4a67452e68b09ae0ee861/packages/effect/src/Scope.ts).
- **Silk inference:** this selects the destination before acquisition. It does not move an already
  installed finalizer from one scope to another. A Silk value already owned by a short region cannot
  become long-lived merely because its handle is moved; it must be acquired in the destination,
  copied or promoted into valid storage, or represented by ownership independent of the old scope.
- **Silk stress test:** try to return an arena-backed value from a helper into a longer-lived scope in
  three ways—moving only the handle, copying into the destination, and acquiring directly in the
  destination—and accept only the latter two.

### 50. Detect the scoped-resource escape that Effect intentionally does not type

- **User problem:** prevent code from using a returned resource after the scope that finalized it has
  closed.
- **Canonical Effect v4 counterexample:**

  ```ts
  const escaped = yield* Effect.scoped(
    Effect.acquireRelease(openFile, closeFile)
  )
  // TypeScript still permits this call after closeFile has run.
  yield* escaped.read()
  ```

- **Effect v4 behavior:** `Effect.scoped` removes the `Scope` requirement and returns the success
  value `A`; the `A` type is not parameterized by the fresh scope's identity. Finalizers run when the
  scoped workflow exits, but the type does not prevent the JavaScript value from escaping.
- **Primary source:** [Effect v4 beta `Effect.scoped` and `acquireRelease`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Effect.ts),
  [Effect v4 beta `Scope.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Scope.ts).
- **Silk inference:** this is not a defect in Effect's JavaScript contract; libraries commonly hide
  invalid post-close operations behind runtime behavior. It is insufficient for Silk, where a borrow
  into reclaimed storage would be memory-unsafe. Silk must either bind the returned type to the
  region, forbid such escape structurally, or require promotion to an independent owner.
- **Silk stress test:** acquire a borrowed slice and an independently owned copy in the same child
  scope. Reject the slice after child close while allowing the copy to escape to the parent.

### 51. Keep a stateless capability separate from the resources its calls create

- **User problem:** provide one file-system interface for the application without pretending that
  every file handle has the same lifetime as the service object.
- **Canonical Effect v4 expression:**

  ```ts
  const program = Effect.gen(function*() {
    const fs = yield* FileSystem.FileSystem
    const info = yield* fs.stat(path) // no Scope requirement
    const file = yield* fs.open(path) // requires the caller's Scope
    return { info, file }
  })

  const FileSystemLive = Layer.succeed(FileSystem.FileSystem, implementation)
  ```

- **Effect v4 behavior:** `FileSystem` is a capability interface. Ordinary operations such as
  `stat` return effects without a scope requirement, while `open`, scoped temporary files, and scoped
  temporary directories explicitly require `Scope`. `Layer.succeed` can provide an already-built
  implementation with no acquisition lifecycle. The official Node layer happens to use
  `Layer.effect` because it effectfully assembles an implementation and consults an optional watch
  backend; file handles are still acquired in each caller's scope, not the layer's scope.
- **Primary source:** [Effect v4 beta `FileSystem.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/FileSystem.ts),
  [Effect v4 beta `Layer.succeed`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Layer.ts),
  [official Node FileSystem layer](https://github.com/Effect-TS/effect/blob/dff25449dfc927f2cce912c329f343cfb5365f88/packages/platform-node-shared/src/NodeFileSystem.ts).
- **Silk inference:** whether service construction is effectful does not by itself make the service
  stateful or resource-owning. Silk should expose the lifetime on the operation that creates the
  resource. A long-lived `FileSystem` capability may safely create handles in many independently
  named request scopes.
- **Silk stress test:** provide one static file-system capability to two concurrent request effects;
  open files in separate child scopes, close one request early, and prove the other service access
  and file handle remain valid.

### 52. Tie a stateful service to the scope that owns its connection

- **User problem:** construct Redis once, keep its connection across many calls, and disconnect it
  exactly when the provider lifetime ends.
- **Canonical Effect v4 expression:**

  ```ts
  const makeRedis = Effect.gen(function*() {
    const scope = yield* Effect.scope
    const client = new RedisClient(options)
    yield* Scope.addFinalizer(scope, Effect.promise(() => client.quit()))
    return Context.make(Redis, { client, use: (f) => useClient(client, f) })
  })

  const RedisLive = Layer.effectContext(makeRedis)
  ```

- **Effect v4 behavior:** the official Node Redis integration constructs an `ioredis` client,
  registers `client.quit()` in the layer's scope, and returns services that close over that client.
  Its operations use the already-acquired connection and do not require callers to supply a fresh
  scope for every command.
- **Primary source:** [official v4 beta Node Redis integration](https://github.com/Effect-TS/effect/blob/dff25449dfc927f2cce912c329f343cfb5365f88/packages/platform-node/src/NodeRedis.ts),
  [Effect v4 beta `Layer.effectContext`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Layer.ts).
- **Silk inference:** this is genuinely resource-owning service state. Any effect using the service
  must be prevented from outliving the provider scope, even if an individual command allocates no
  scoped result. A child operation's temporary buffers may still belong to a shorter request scope.
- **Silk stress test:** share one connection-owning Redis provider across child requests, give each
  request independent scratch and response scopes, interrupt one request, and close the connection
  only after all provider-borrowing children terminate.

### 53. Keep construction, service use, and result-resource requirements distinct

- **User problem:** tell whether a requirement is needed to build a service, to call the service, or
  to own a resource returned by one particular call.
- **Canonical Effect v4 shape:**

  ```ts
  // Static implementation: no construction effect or construction scope.
  const staticLayer = Layer.succeed(Service, implementation)

  // Effectful/scoped construction: Config remains an input; Scope is owned by Layer.
  const liveLayer = Layer.effect(Service, makeService)

  // Per-call resource: Scope belongs to the caller of open, not necessarily the layer.
  interface ServiceShape {
    readonly open: (path: string) => Effect.Effect<Handle, OpenError, Scope.Scope>
  }
  ```

- **Effect v4 behavior:** `Layer<Out, E, In>` records construction output, construction failure, and
  unsatisfied construction inputs. In beta.102, `Layer.effect` and `Layer.effectContext`
  automatically provide their build scope and exclude `Scope` from `In`; other construction
  requirements remain. This consolidates the v3 distinction between `Layer.effect` and
  `Layer.scoped`. The layer lifetime and memoization model remain; the change is chiefly API
  simplification, not a semantic merger of service lifetime with every operation's resources.
- **Primary source:** [Effect v4 beta `Layer.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Layer.ts),
  [Effect v3.22.1 `Layer.effect` and `Layer.scoped`](https://github.com/Effect-TS/effect/blob/417e0faa80e471d77fc4a67452e68b09ae0ee861/packages/effect/src/Layer.ts).
- **Silk inference:** Silk should resist one undifferentiated notion of "service scope." A complete
  example may contain at least three lifetimes: temporary capabilities used only during provider
  construction, state retained by the provider, and resources produced by individual methods.
  Their requirements and ownership must remain separately visible even if surface syntax makes the
  common case concise.
- **Silk stress test:** build a connection pool from configuration and a bootstrap allocator, release
  construction-only scratch, keep pool state for the application scope, and acquire a transaction
  plus response buffer in a shorter request scope.

### 54. Give each service call's result an independently chosen scope

- **User problem:** reuse one service across application, request, and temporary scopes while placing
  each returned resource in the narrowest correct lifetime.
- **Canonical Effect v4 expression:**

  ```ts
  const readIn = (scope: Scope.Scope, path: string) =>
    Effect.gen(function*() {
      const fs = yield* FileSystem.FileSystem
      return yield* fs.open(path).pipe(Scope.provide(scope))
    })
  ```

- **Effect v4 behavior:** service lookup and scope provision are independent requirements. The same
  `FileSystem` implementation can serve calls whose `open` acquisitions are registered in different
  explicit scopes.
- **Primary source:** [Effect v4 beta `FileSystem.open`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/FileSystem.ts),
  [Effect v4 beta `Scope.provide`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Scope.ts).
- **Silk inference:** the service requirement should not silently choose an allocator or result
  lifetime. Callers need a way to select or infer the result scope, and the returned resource type
  must retain that choice strongly enough to prevent escape.
- **Silk stress test:** use one stateless capability to create an application-lived cache file, a
  request-lived input handle, and a block-lived scratch mapping; check that each result closes at its
  own boundary and cannot be used afterward.

## Cross-cutting adversarial scenarios

These scenarios combine several patterns and are more revealing than isolated feature demos.

1. **Retrying parser with scratch allocation:** every attempt allocates scratch space, may fail with
   one of two nominal errors, records a span, and returns an independently owned syntax tree on
   success. Failed attempt memory must be reclaimed before delay and retry.
2. **Raced replicas returning owned buffers:** two providers allocate output concurrently. The first
   success wins; the losing task is interrupted and cleaned; the returned buffer remains valid.
3. **Timeout during provider acquisition:** acquisition crosses interruptible and uninterruptible
   phases. Timeout must never expose a half-initialized provider or lose the original failure.
4. **Scoped provider used by child tasks:** a provider owns an arena shared by several child
   executions. The providing scope cannot close until children finish or are interrupted and joined.
5. **Bounded streaming compiler pipeline:** the reader, parser, analyzer, and encoder exchange owned
   chunks through bounded queues. Backpressure bounds allocation, and failure in one stage shuts down
   the rest without stranded values.
6. **Batched requests with per-batch arena:** individual queries remain typed and composable; a
   resolver groups them, allocates one batch buffer, completes every request exactly once, and frees
   batch storage after results have been moved or copied to valid owners.
7. **Cache eviction with active readers:** one task reads a cached value while another invalidates or
   refreshes it. The design must make the read value's ownership explicit instead of relying on GC.
8. **Failure payload owns allocation:** a computation fails with an affine diagnostic containing
   owned text. A matching handler may inspect, transform, re-fail, or consume it, with one cleanup
   path in every branch.
9. **Repeated lazy computation capturing a vector:** construction captures the vector by shared
   borrow, exclusive borrow, or move. Execution access and number of legal runs must follow capture
   semantics rather than an unrelated reusable/single-shot marker.
10. **Dynamic finalizers inside a loop:** each iteration may acquire an owned resource, fail, break,
    continue, or finish. Cleanup order and live-set bounds must remain deterministic.
11. **Allocator maximum versus early logical drop:** one parent allocator scope owns several child
    object scopes. Drop one object's external resources and owned children immediately after its last
    use without pretending the arena returned its bytes, then reject every later use of the object.
12. **Sibling compiler phases with unequal lifetimes:** parsing, analysis, and emission have named
    child scopes under one compilation scope. Promote only independently owned facts across phase
    boundaries and close each completed phase before the next reaches peak allocation.
13. **Attempted scope promotion by handle move:** acquire a view in a short scope, move its handle to
    a longer scope, and verify this alone cannot move its finalizer or backing storage. Compare with
    copying or acquiring directly in the destination.
14. **Stateless service with scoped results:** share one file-system capability across concurrent
    requests while every opened file belongs to its request scope. Closing one request must neither
    invalidate the service nor affect another request's handle.
15. **Stateful service with shorter call resources:** keep one Redis connection in an application
    provider scope while commands allocate temporary request buffers. Request failure cleans those
    buffers but does not disconnect the shared provider; provider shutdown waits for borrowing child
    executions.
16. **Construction scratch, provider state, and returned handle:** build a service using temporary
    scratch, retain a connection in the provider, then return a transaction in a request scope. Test
    all three lifetimes independently instead of treating them as one "service scope."

## Callable composition scenarios selected for Silk

The callable review resolves a gap in the earlier corpus: pipelines are not argument-insertion
syntax, and Effect combinators do not receive a privileged callback category. Named functions and
automatic leading-argument sections are ordinary values with compiler-derived environments.

1. **Reusable Copy mapper:** `succeed(2) |> Effect.map(I32.add(40))` snapshots `40`; the section is
   shared reusable and can also be stored and called as `I32.add(40)(2)`.
2. **Exclusive stateful mapper:** a section such as `increment(&mut state)` retains one exclusive
   borrow. The mapped Effect requires exclusive execution, sequential runs observe the same state,
   and dropping the composition ends the loan.
3. **Consuming mapper:** `consume(move allocation)` owns the Allocation in its environment. Mapping
   with it makes the composed Effect take-once, retry is rejected before execution, and dropping the
   unrun Effect releases the allocation exactly once.
4. **Generic section:** trailing capture evidence specializes what it can; the omitted leading
   argument supplies the remaining generic evidence at application. Expected results and later uses
   never participate in inference.
5. **Effectful logging:** a Logger-requiring callback is passed to `tap` or `flatMap`, not hidden in a
   pure `map` or compiler tracing intrinsic. Its requirement row propagates to the composed Effect,
   and a provider is supplied at the program boundary.
6. **Nested map:** when the callback returns an Effect, `map` preserves `Effect<Effect<A>>`; a second
   `run` or explicit flattening is required. `flatMap` removes exactly one layer and `tap` preserves
   the original success.
7. **Grouped versus ungrouped run:** `run effect |> Effect.map(callback)` composes before executing
   because `run` owns the complete following expression. `(run effect) |> transform` executes first
   and applies an ordinary callable to the eager success value.
8. **Borrowed resource adapter:** a stored shared or exclusive section may cross ordinary function
   borders only while its captured loan remains valid. Its structural `fn` or `mut fn` contract
   hides environment layout but not invocation guarantees.

These scenarios deliberately require no universal closure box. Hidden construction identity,
ownership, instance discovery, target-aware layout, and the backend-neutral MIR DAG keep every
environment concrete. Unknown-sized owned erased callable returns and heterogeneous callable
storage remain deferred.

## What this corpus suggests measuring before choosing syntax

The examples repeatedly depend on a small semantic kernel. A syntax candidate should be judged by
whether users can see and predict these facts:

1. Which code runs at ordinary function call, lazy-value construction, and lazy-value execution.
2. Which values are captured at construction and whether each is copied, borrowed, or moved.
3. Whether the resulting value accepts shared execution, exclusive execution, or one consuming run.
4. Which failures are typed values, which conditions are traps, and how interruption is represented.
5. Which capability requirements remain open and when provider acquisition occurs.
6. Which scope owns every acquired resource and every explicit allocation.
7. What is guaranteed to clean up on success, failure, early return, interruption, race loss, and
   timeout.
8. Whether retry/repeat reconstructs the body, reuses captures, reacquires providers, or does some
   combination of the three.
9. How child execution lifetime relates to captured borrows, providers, and parent scopes.
10. Whether retaining abstractions such as queues, caches, and batches own values, borrow them, or
    require explicit duplication.
11. Whether a scope name is merely a runtime handle or a static region identity carried by values.
12. Whether logical drop, finalizer execution, and allocator storage reclamation occur together or
    at deliberately different boundaries.
13. Whether a requirement belongs to provider construction, provider use, or a resource returned by
    one operation, and which scope discharges each requirement.

The Effect corpus is strongest evidence for the usefulness of laziness, typed failures,
requirements, scope, interruption, concurrency, scheduling, and observability as one compositional
model. It is weakest exactly where Silk is most ambitious: affine captures, explicit memory
allocation, escape checking, and target-aware layout. Those gaps are not reasons to discard the
patterns; they are the constraints the next design round must make visible.

The original v3 examples remain stable semantic references but are pending a mechanical v4 surface
rewrite; that rewrite is intentionally separate from this scope and service-lifetime investigation.
Advanced categories deliberately left for a later expansion include pub/sub fan-out, mutable effect
state, scoped local service overrides, resource pools, STM, supervisors, and durable workflows. They
should be tested only after the kernel above has a coherent ownership story; none is needed to
compare the immediate effect syntax candidates.

## Primary-source index

- [Effect v4 beta.102 `Effect.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Effect.ts)
- [Effect v4 beta.102 `Scope.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Scope.ts)
- [Effect v4 beta.102 `Layer.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Layer.ts)
- [Effect v4 beta.102 `Context.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/Context.ts)
- [Effect v4 beta.102 `FileSystem.ts`](https://github.com/Effect-TS/effect/blob/de2a9a69099993087e57c64df58537c765ac0224/packages/effect/src/FileSystem.ts)
- [Official Node FileSystem beta.103 source](https://github.com/Effect-TS/effect/blob/dff25449dfc927f2cce912c329f343cfb5365f88/packages/platform-node-shared/src/NodeFileSystem.ts)
- [Official Node Redis beta.103 source](https://github.com/Effect-TS/effect/blob/dff25449dfc927f2cce912c329f343cfb5365f88/packages/platform-node/src/NodeRedis.ts)
- [Effect v3 documentation](https://effect.website/docs/v3/)
- [Effect v3 API reference](https://effect.website/docs/v3/api)
- [Effect v3.22.1 `Scope.ts`](https://github.com/Effect-TS/effect/blob/417e0faa80e471d77fc4a67452e68b09ae0ee861/packages/effect/src/Scope.ts)
- [Effect v3.22.1 `Layer.ts`](https://github.com/Effect-TS/effect/blob/417e0faa80e471d77fc4a67452e68b09ae0ee861/packages/effect/src/Layer.ts)
- [Official Effect repository at the original research commit](https://github.com/Effect-TS/effect/tree/269516851b24916d72771f8a554b88722e3732e7)
