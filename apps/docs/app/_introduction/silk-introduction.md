# What If Effect Were a Low-Level Language?

## Composability, all the way down

Silk is a low-level programming language that started with a question I kept coming back to:

> **Can systems code compose the way Effect programs do, without giving up the explicitness and control that make low-level programming worth using in the first place?**


```silk
import silk.effect { Effect }
import silk.logger { Logger }

pub effect fn main() -> () ! Logger.LogError {
  let mut logger = Logger.stdoutProvider()

  run Effect.log("Hello, world!\n")
    |> Effect.provideMut(&mut logger)
}

```

Yes, this is a fairly verbose way to say “Hello, world!”

I still stand by it. I also do not expect it to stay this verbose.

The reason I like the example is that, for such a tiny program, it already shows a huge amount of the language.

`Effect.log` is not hard-coded to stdout, a global logger, an exception mechanism, or some invisible runtime environment. It builds a lazy description of work. Both its possible `LogError` and its need for a `Logger` are part of the type. `stdoutProvider` is just one implementation of that capability. `provideMut` supplies it through an exclusive borrow, and the inner `run` is the point where the description finally becomes execution.

Until then, logging is still an abstract operation. The application gets the final say on what it means.

There is another idea hiding in this tiny example:

> **Dependency injection and memory management are not separate universes. They obey the same ownership rules.**

Some of this ceremony is simply a stage-0 limitation. Every service requirement that reaches an executable entry point currently has to be provided explicitly. Effect functions can propagate requirements declared by their contracts, but the compiler does not yet install overridable platform defaults at the program boundary.

## Why another programming language?

> **Michael Arnaldi**
>
> What am I looking at????
>
> **Julia**
>
> it's a compiler I'm writing that tries to port Effect concepts to a low level language, with a llvm backend, written in..Effect
>
> Curious to know your thoughts.
>
> **Michael Arnaldi**
>
> it's pure insanity but you know that
>
> it's also amazing lol
>
> — Discord, 11 August 2026

**Why did I decide to write a low-level language at the exact historical moment when everyone seems to have decided humans should stop writing code?**

The honest answer is: because I wanted to. I am not trying to change the world. I am a side-project addict, not fucking Margaret Hamilton.

But it was not completely random. There was a real itch behind it.

We spend our whole careers saying software should be composable and reusable. In practice, those goals often end up next to TDD and permanently current internal docs: things we genuinely value, genuinely try to do well, and eventually accept a “best effort” version of.

Most code composes until reality shows up.

It can fail. It needs something from the environment. It owns a resource, allocates memory, starts concurrent work, or needs to be interrupted safely. Each concern tends to arrive with its own mechanism and conventions, and every new mechanism adds more glue.

Effect was the first thing that changed how this felt to me.

Composition was no longer merely possible, or something a sufficiently disciplined programmer could preserve by following enough rules. It became the path of least resistance: easy, natural, almost unavoidable.

A computation could say what it returns, what can go wrong, and what it needs from the world. Those pieces could be transformed, combined, provided, recovered, and reused without every new concern punching a hole through the abstraction.

I wanted that feeling in a low-level language.

Zig had already taught me a different lesson: low-level programs should tell the truth. Allocation, control flow, ownership, and cost should not disappear behind friendly-looking syntax.

That was not the pain that made me start Silk. It became the condition I put on the solution.

> **Silk asks whether a language can make programs genuinely composable without lying about the machine underneath them.**

## Effects: the unit of composition

The idea at the center of Effect is small: a computation’s type describes not only what it can produce, but also how it can fail and what it needs from the world.

```text
Effect<A ! E ? R>
       │   │   │
       │   │   └── what the computation requires
       │   └────── how the computation may fail
       └────────── what the computation may produce

```

For example:

```silk
//  A: eventually produces an i32
// !E: may fail with BootError
// ?R: requires shared access to Clock
//     and exclusive access to Logger

effect fn boot() -> i32
! BootError
? &Clock | &mut Logger

```

When Effects are combined, those three parts compose independently. Calling another function does not make its failures or dependencies magically disappear.

### Description before execution

Effects in Silk are lazy in the literal sense. Calling an `effect fn` builds an `Effect`; it does not enter the function body yet. `run` executes exactly one layer, and nested `Effect<Effect<A>>` values are not silently flattened for you.

An ordinary function can still do some eager setup and then return a deferred `effect { ... }` block:

```silk
pub fn main() -> i32 {
  let eager = 21

  let delayed = effect {
    //          └──────── constructs
    return eager + 21
  }

  return run delayed
  //     └────────────── executes
}

```

Effects are ordinary owned values too. What they capture determines whether they can be reused or must be consumed once. Dropping an Effect without running it releases those captures; the body never executes.

> **An Effect is a recipe whose type includes the result, the possible failure, and the ingredient list.**

That gap between describing work and executing it is where much of the reuse comes from. You can build an Effect, transform it, provide dependencies, recover from failures, or combine it with other work before anyone commits to actually running it.

## Failures that stay inside composition

Failure is often the first place a reusable abstraction splits in half. Success stays in the normal flow; failure escapes through a completely different mechanism.

Silk keeps recoverable failure inside the Effect model instead.

This is already a complete typed failure:

```silk
effect fn load() -> i32 ! string {
  fail "not found"
}

```

A failure payload does not have to inherit from an exception hierarchy, implement an `Error` marker, or belong to some privileged class of error objects.

In Silk, error payloads are just values. Effects give those values a typed failure channel.

Named error types are still useful when the distinction itself matters:

```silk
import silk.effect { Effect }

struct NotFoundError {
  id: i32
}

effect fn load(id: i32) -> i32 ! NotFoundError {
  fail NotFoundError { id: id }
}

effect fn recover(error: NotFoundError) -> i32 {
  return 0
}

pub fn main() -> i32 {
  return run load(42)
             |> Effect.catch(recover)
}

```

`recover` receives an ordinary owned `NotFoundError`. It can be mapped, matched, selectively recovered, propagated, or converted into ordinary `Result<A, E>` data with `Effect.result`. `catch` and `mapError` work directly on the typed failure channel.

Silk also does not add a second propagation operator. If the surrounding Effect permits `NotFoundError`, running `load` already means “give me the value, or propagate the typed failure.”

> **`run` is not “unwrap.” It is composition.**

It executes one Effect layer. On success, the value enters the surrounding computation. On failure, the typed error enters the surrounding Effect contract.

At an ordinary `fn` boundary, both the failure and requirement channels must be closed. An `effect fn` may propagate whatever its own contract declares. Those declarations are upper bounds: the body may use fewer capabilities or failure types than listed, but omitting `! E` means `never`, and omitting `? R` means an empty requirement row, not inferred permission.

An executable `effect fn main` is a special host boundary. It may keep a concrete typed failure, which becomes a reported nonzero process outcome, but it cannot leave a service requirement unresolved.

### Failures are not traps

Typed failures are for conditions the program expects to handle.

Bounds violations, division by zero, trapping arithmetic overflow, invalid runtime states, and violated unsafe contracts are fatal traps. A trap is not part of `E`: it cannot be caught with `catch` or turned into data with `result`, and it may bypass structured cleanup.

If a condition is meant to be recoverable, it has to become ordinary data or a typed failure before reaching that point.

## Services: reuse without concrete implementations

> **A service is the capability. A provider is one implementation of that capability.**

A `service` is an interface that can appear in an Effect requirement. Reusable code asks for the service itself, not the concrete type that will eventually implement it.

```silk
import silk.effect { Effect }

service Clock {
  effect fn value() -> i32 ? &Clock
}

struct FixedClock {
  value: i32
}

effect fn clockValue(self: &FixedClock) -> i32 {
  return self.value
}

impl Clock for FixedClock {
  value: FixedClock.clockValue
}

effect fn readClock() -> i32 ? &Clock {
  return run Clock.value()
}

pub fn main() -> i32 {
  let clock = FixedClock { value: 42 }

  return run Effect.provide(
    readClock(),
    &clock,
  )
}

```

`readClock` never mentions `FixedClock`; it only asks for `Clock`. The concrete provider matters later, when the application decides how to satisfy that capability.

```text
readClock()
    │
    │ requires
    ▼
  &Clock                 ← contract
    ▲
    │ provides
    │
FixedClock { value: 42 } ← implementation

```

The logger from the opening example is another service:

```silk
service Logger {
  effect fn log(
    level: LogLevel,
    message: string,
  ) -> () ! LogError ? &mut Logger
}

```

The contract says nothing about whether a log event goes to stdout, memory, a browser console, a file, telemetry, or something nobody has invented yet. It also says nothing about timestamps, prefixes, newlines, or allocation. Those choices belong to the provider.

```silk
let mut logger = Logger.stdoutProvider()
// or: Logger.inMemoryProvider()
// or any application-defined provider

run Effect.log("connected")
  |> Effect.provideMut(&mut logger)

```

Tests do not need a separate universe of flimsy “mock objects.” An in-memory provider is simply another real implementation of the capability.

This is the kind of reuse I care about: the operation keeps its meaning and contract, while the application stays free to decide how that operation is interpreted.

### One service, multiple roles

Sometimes one computation needs more than one instance of the same service. Every requirement therefore has a role. The default role stays implicit, but you can name it when the distinction matters:

```silk
role Main
role Scratch

effect fn prepareBuffers() -> ()
? &Allocator at Main
| &Allocator at Scratch {
  return ()
}

```

`Allocator at Main` and `Allocator at Scratch` are different dependency keys, despite referring to the same service.

```silk
let pending = prepareBuffers()
  |> Effect.provide<Allocator at Main>(&systemAllocator)
  |> Effect.provide<Allocator at Scratch>(&arenaAllocator)

return run pending

```

The access mode is part of the requirement too:

```silk
? &Clock

```

means shared provider access, while:

```silk
? &mut Logger

```

means exclusive provider access.

Requirement rows are normalized, unordered, and duplicate-free. Service, role, and access mode together identify the exact capability being requested.

Providing a service does not create a loophole in the borrow checker. It still borrows or captures a provider, and the normal ownership and lifetime rules decide whether that is legal.

> **Dependency injection is not a magical runtime side channel. It participates in Silk’s ownership system.**

Provision is lexical; it does not mutate a global container. It removes one exact service, role, and access requirement from one Effect layer. If that Effect later returns another Effect as ordinary data, the inner Effect is not silently provided as well.

## Ownership: composition survives resource boundaries

Resource boundaries are another place where abstractions often stop composing. A function may be perfectly reusable in what it computes while still baking in who owns the result, where its memory comes from, or when cleanup happens.

Silk tries to keep those questions in the same model instead of treating them as a separate low-level afterthought.

It borrows—yes, the pun is unavoidable—from Rust-style ownership and deterministic cleanup, Zig-style allocator explicitness, and Effect-style dependency tracking.

Values are either `Copy` or affine. User-defined structs are affine unless they explicitly request valid `Copy` conformance. You can borrow or transfer an affine value, but you cannot silently duplicate it.

```silk
struct Message {
  code: i32
}

fn inspect(message: &Message) -> i32 {
  return message.code
}

fn consume(message: Message) -> i32 {
  return message.code
}

pub fn main() -> i32 {
  let message = Message { code: 42 }

  let code = inspect(&message)        // shared borrow
  return code + consume(move message) // transfer ownership
}

```

The vocabulary is deliberately visible:

```text
&message       borrow it
&mut message   borrow it exclusively
move message   give it away

```

After `move message`, the original owner is no longer available on that control-flow path. A shared borrow does not transfer ownership, while `&mut` grants exclusive mutation access. Deterministic cleanup follows ownership.

Silk is currently more restrictive than Rust in a few places. Stage-0 Silk, for example, forbids partial moves out of aggregates that remain alive:

```silk
struct Token {
  kind: i32
}

struct Envelope {
  token: Token
  code: i32
}

fn invalid(envelope: Envelope) -> Token {
  return move envelope.token
}

```

### The allocator is a service

One thing Zig got exactly right is that allocation policy should not be an invisible library decision. Silk keeps that explicitness, but puts allocator access in the same Effect requirement system as every other capability.

The standard-library service looks roughly like this:

```silk
service Allocator {
  effect fn allocate(layout: Layout)
    -> Allocation
    ! OutOfMemoryError
    ? &mut Allocator
}

```

There is not much left to guess from that signature:

```text
returns       Allocation
can fail      OutOfMemoryError
requires      exclusive Allocator access

```

`SystemAllocator` is one concrete process-backed provider. If allocation is refused, that is an ordinary typed failure.

Library operations that allocate can expose the same facts directly. `Box.make` currently looks like this:

```silk
effect fn make<T>(value: T)
  -> Box<T>
  ! OutOfMemoryError
  ? &mut Allocator

```

The application then chooses the allocation policy at the boundary:

```silk
import silk.allocator { Allocator }
import silk.box { Box }
import silk.effect { Effect }

effect fn boxedAnswer() -> i32
! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()

  let creating = Box.make<i32>(42)
    |> Effect.provideMut<Allocator>(&mut allocator)

  let boxed = run creating

  return Box.into<i32>(move boxed)
}

```

`Box.make` makes the allocation visible. `provideMut` chooses the allocator, `run` produces one affine owner, and `Box.into(move boxed)` consumes that owner to move the contained value back out.

`Box` itself is ordinary Silk source, not a compiler-recognized heap primitive. It owns a value and an allocation, then coordinates destruction so both are released exactly once.

One subtle detail matters here: a successful low-level `Allocation` is a self-contained affine owner. It does not keep a borrow of the allocator that created it. The allocator loan can end immediately after allocation, while the returned `Allocation` moves independently with everything it needs to release itself later.

Silk also has allocation-backed local shared ownership through `Shared<T>`. Strong handles are still affine, access is callback-scoped, and the current abstraction is local rather than thread-transferable. This is what the current Fiber and scheduler implementation uses for shared state.

> **Ownership is syntax you can read. The allocator is a service.**

The point is not to make code explicit as a ritual. It is to let allocation policy and resource ownership cross abstraction boundaries without turning into hidden global decisions, and without destroying the composability of the operation that needs them.

## Fibers: composition survives concurrency

Concurrency is usually where one clean model turns into a second, messier one: callbacks, task IDs, cancellation flags, scheduler globals, detached lifetimes, and a fresh set of errors that compose with nothing else.

Effect convinced me that concurrent work did not have to leave the main computational model. Silk asks what that looks like when both tasks and the right to observe them are owned resources.

Zig brings the same obsession with explicitness to concurrency that it brings to allocation. I have watched Andrew Kelley painstakingly search for the right async and I/O abstraction over the years. Naturally, because I am much smarter than him, [I stole mine from Effect](https://www.effect.website/docs/v4/concurrency/basic-concurrency).

Right now, Silk has a **local, cooperative, single-threaded structured-concurrency model** built from `Scheduler`, `Fiber`, `Execution`, and `Wake`.

A scheduler owns the root task and every task below it. Each task runs until it completes, parks, or yields. `Fiber.forkChild` creates structured child work; `Fiber.await` and `Fiber.join` consume an affine authority to observe that work.

```silk
import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
import silk.fiber { Fiber } 
import silk.local_scheduler { LocalScheduler }
import silk.scheduler { Scheduler }

effect fn work() -> i32 {
  return 42
}

effect fn program() -> i32
! OutOfMemoryError
| Scheduler.TaskIdExhaustedError
| Fiber.Cancelled
? &mut Scheduler.Scheduler {
  let child = run Fiber.forkChild<i32, never>(work())
  return run Fiber.join<i32, never>(move child)
}

effect fn recover(
  error: OutOfMemoryError
    | Scheduler.TaskIdExhaustedError
    | Fiber.Cancelled
    | LocalScheduler.StalledError,
) -> i32 {
  drop error
  return -1
}

pub fn main() -> i32 {
  let mut scheduler = LocalScheduler.make()

  return run Effect.catchAll(
    LocalScheduler.execute(&mut scheduler, program()),
    recover,
  )
}

```

The whole idea fits in three lines:

```text
forkChild(...)        create structured child work
Fiber<A, E>           receive an affine authority to observe it
join(move child)      consume that authority exactly once

```

A Fiber handle owns one completion observer. `await` returns `Fiber.Outcome<A, E>`, so success, typed failure, and cancellation become ordinary data. `join` takes the other path: it returns `A` on success and propagates the child’s `E` or `Fiber.Cancelled`.

Dropping the handle means giving up the right to observe completion. It does not detach the child or cancel it independently; the child remains structurally linked to the task that created it.

> **A Fiber is not a task ID. It is an owned capability to observe a task.**

That brings concurrency back into the ownership model and keeps it inside the same compositional structure as every other Effect.

### The scheduler is explicit too

Silk does not silently pick a scheduler just because an Effect can park. The application constructs one and enters it explicitly. `LocalScheduler.execute` takes ownership of the root Execution and drives it. The local scheduler shipped today uses deterministic FIFO readiness.

`Scheduler` itself is also a service. In simplified form:

```silk
service Scheduler {
  effect fn prepare<A, E>(
    child: once Effect<
      A ! E ? &mut Scheduler
    >
  ) -> PendingPublication<A, E>
    ! OutOfMemoryError
    | TaskIdExhaustedError
    ? &mut Scheduler
}

```

Alternative local scheduling policies can therefore be ordinary providers. The compiler only supplies the target-neutral mechanisms needed for execution, parking, waking, and lifecycle management. Fibers, queues, readiness policy, task storage, and scheduling all live in ordinary Silk source.

> **Mechanism belongs to the runtime. Policy belongs in Silk.**

Structured concurrency keeps the child lifetime tree visible:

```text
                root
                 │
          ┌──────┴──────┐
          │             │
       Fiber A       Fiber B
          │
       Fiber C

```

When the root terminates:

```text
root terminates
      │
      ▼
unfinished descendants cancel

```

Whether the parent succeeds, fails, is cancelled, or stalls during shutdown, unfinished descendants are cancelled and released before the parent’s final outcome becomes observable. The initial API deliberately has no detached tasks, daemon tasks, or reparenting operation.

Creating a child may expose `OutOfMemoryError` and `Scheduler.TaskIdExhaustedError`. Joining may expose the child’s own `E` plus `Fiber.Cancelled`. Concurrency does not get a separate exception universe; its recoverable operational failures remain ordinary Effect failures.

```text
forkChild
    ↓
Fiber<A, E>
    ↓
join
    ↓
A ! E | Cancelled

```

> **Concurrency does not get to escape the Effect model either.**

Not every invalid concurrency state is recoverable. Lifecycle violations and exhaustion of the private execution stack remain fatal traps outside the Effect failure channel.

**A few current execution-model details**

The current `LocalScheduler` constructs a `SystemAllocator` provider internally for task storage. Allocation refusal is still typed, but callers do not supply an `Allocator` requirement to `execute` or `forkChild`.

Independently resumable root and child Effects cannot retain an arbitrary service environment borrowed from the caller. Work passed to `execute` or `forkChild` must be closed except for the owned Scheduler capability. Any other dependency has to be provided in a form whose ownership can cross the Execution boundary.

A `Wake` is an opaque, affine, generation-scoped authority to publish readiness once. Consuming it may make an Execution eligible to be driven later, but it does not execute the task inline or carry the task’s result. A late Wake retained after cancellation is inert.

`Execution`, `Wake`, and `Shared` are currently local values. Silk provides cooperative, single-threaded scheduling, not parallel or multithreaded fibers. Thread transfer, synchronization, atomic readiness delivery, and parallel scheduling are future work.

## What now?

Silk is nowhere near production, and that is fine. It is ready to be experimented with, argued about, broken, and redesigned.

The APIs will move. Networking, broad FFI, a package registry, self-hosting, and multithreaded scheduling are still ahead. The compiler and standard library are far enough along to test the idea, but not far enough along to pretend the language is finished.

The experiment comes down to a few rules:

- When computations combine, their success, failure, and requirements should combine too.
- Reusable code should depend on a capability, not one particular implementation.
- If an abstraction allocates, the caller should still be able to choose the allocation policy.
- When ownership moves, the syntax should make it clear who is now responsible for the resource.
- When concurrent work is created, its errors, dependencies, and lifetime should remain part of the same program.

Silk is an experiment in whether composability can survive contact with the machine.

The goal is not abstraction at any cost, and it is not explicitness as a virtue by itself.

> **The goal is to make the composable program and the honest program the same program.**