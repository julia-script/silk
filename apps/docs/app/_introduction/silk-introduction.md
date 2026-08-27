# What If Effect Were a Low-Level Language?

Silk is a low-level programming language built around a simple question:

> **What happens when the ideas behind Effect are pushed all the way down to systems programming?**

The result is a language where a computation can describe not only what it returns, but what can fail, which capabilities it needs, who owns its resources, which allocator it uses, and how concurrent work is supervised.

**Typed effects and dependencies. Explicit failures. Affine ownership and borrowing. Explicit allocation. Structured concurrency. No ambient runtime magic.**

> **Status: stage 0.** Silk is unreleased and deliberately unstable. Today, the compiler can interpret its logical MIR, emit native code or WebAssembly through LLVM, or emit WebAssembly directly without LLVM. Networking, a package registry, broad FFI, self-hosting, and multithreaded scheduling are still future work.

## Hello, world—honestly

```silk
import silk.effect { Effect }
import silk.logger { Logger }

pub effect fn main() -> () ! Logger.LogError {
  let mut logger = Logger.stdoutProvider()

  run Effect.log("Hello, world!\n")
    |> Effect.provideMut(&mut logger)
}
```

Yes, this is a rather verbose way to say “Hello, world!”.

First, **I stand by it.** Second, **I do not expect it to stay this verbose.**

I stand by it because this tiny program already explains an unreasonable amount of the language.

`Effect.log` constructs a lazy computation. Its possible `LogError` is part of its type. Its dependency on `Logger` is part of its type. `stdoutProvider` is merely one implementation of that capability. `provideMut` supplies it through an exclusive borrow. The inner `run` is where the description finally becomes execution.

And hidden inside that small example is one of Silk’s central ideas:

> **Dependency injection and memory management are not separate universes. They obey the same ownership rules.**

The verbosity is temporary because every service requirement that reaches an executable entry point must currently be provided explicitly. Effect functions may propagate requirements declared by their own contracts, but the compiler does not yet install overridable platform defaults at the program boundary.

## Why another programming language?

> **Michael Arnaldi**  
> What am I looking at????
>
> **Julia**  
> it's a compiler I'm writing that tries to port Effect concepts to a low level language, with a llvm backend, written in..Effect  
> Curious to know your thoughts.
>
> **Michael Arnaldi**  
> it's pure insanity but you know that  
> it's also amazing lol
>
> — Discord, 11 August 2026

**Why did I decide to write a low-level language at the exact historical moment when everyone has decided humans should stop writing code?**

The honest answer is: because I wanted to. I am a side-project addict, not fucking Margaret Hamilton.

But there is a real conviction underneath the lack of self-control: **the less code we write ourselves, the more I want the code that remains to tell the truth.**

Writing is getting cheaper. Understanding is not. If anything, explicitness matters more now—not less.

Effect has shown a compelling way to describe computations: not merely by what they return, but by what can go wrong and what they need from the world.

Silk asks what happens when you push that idea all the way down.

## Effects: one signature, three answers

A Silk function signature does not stop at the return value. It can answer three questions:

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

Success, failure, and requirements compose independently. None of them disappears merely because another function called this one.

### Description before execution

Effects in Silk are genuinely lazy. Calling an `effect fn` constructs an `Effect`; it does not enter the function body. `run` executes exactly one layer, and nested `Effect<Effect<A>>` values are not silently flattened.

An ordinary function may also perform eager setup and return a deferred `effect { ... }` block:

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

Effects are ordinary owned values too. Their captures determine whether they may be reused or must be consumed once. Dropping an Effect that was never run releases its captures without executing its body.

> **An Effect is a recipe whose type includes the result, the possible failure, and the ingredient list.**

## Failures are values

This is a complete typed failure:

```silk
effect fn load() -> i32 ! string {
  fail "not found"
}
```

Silk does not require failure payloads to inherit from an exception hierarchy, implement an `Error` marker, or belong to a special class of error objects.

There are values, and an Effect has a failure channel.

Nominal errors are still useful when the distinction itself carries meaning:

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

`recover` receives an ordinary owned `NotFoundError`. The value may be mapped, matched, selectively recovered, propagated, or converted into ordinary `Result<A, E>` data with `Effect.result`.

Silk does not add a second propagation operator. When the surrounding Effect permits `NotFoundError`, running `load` already means either receiving its success or propagating its typed failure.

> **`run` is not “unwrap.” `run` is composition.**

At an ordinary `fn` boundary, failures and requirements must both be closed. An `effect fn` may propagate whatever its contract declares. An executable `effect fn main` is a special host boundary: it may retain a concrete typed failure, which becomes a reported nonzero process outcome, but it may not retain an unresolved service requirement.

### Failures are not traps

Typed failures are for conditions the program intends to handle.

Bounds violations, division by zero, trapping arithmetic overflow, invalid runtime states, and violated unsafe contracts are fatal traps. A trap is not part of `E`; it cannot be caught with `catch` or turned into data with `result`, and it may bypass structured cleanup.

A recoverable condition must become ordinary data or a typed failure before it traps.

## Services: capabilities, not implementations

> **A service is the capability. A provider is one implementation of that capability.**

A `service` is an interface that may appear in an Effect requirement. Effects depend on the service itself, never on the concrete type chosen to implement it.

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

`readClock` never mentions `FixedClock`. It asks for `Clock`. The concrete provider becomes relevant only when the application decides how to satisfy that capability.

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

The contract does not decide whether a log event goes to standard output, memory, a browser console, a file, telemetry, or something invented tomorrow. It does not mandate timestamps, prefixes, newlines, or allocation. Those are provider decisions.

```silk
let mut logger = Logger.stdoutProvider()
// or: Logger.inMemoryProvider()
// or any application-defined provider

run Effect.log("connected")
  |> Effect.provideMut(&mut logger)
```

Tests do not need a separate universe of “mock objects.” An in-memory provider is simply another real implementation of the same capability.

### One service, multiple roles

Sometimes one computation needs more than one instance of the same service. Every requirement therefore carries a role. The default role is implicit, but it may be named when necessary:

```silk
role Main
role Scratch

effect fn prepareBuffers() -> ()
? &Allocator at Main
| &Allocator at Scratch {
  return ()
}
```

`Allocator at Main` and `Allocator at Scratch` are different dependency keys, even though both refer to the same service.

```silk
let pending = prepareBuffers()
  |> Effect.provide<Allocator at Main>(&systemAllocator)
  |> Effect.provide<Allocator at Scratch>(&arenaAllocator)

return run pending
```

Access strength belongs to the requirement as well:

```silk
? &Clock
```

means shared provider access, while:

```silk
? &mut Logger
```

means exclusive provider access.

Provisioning does not bypass the borrow checker. It borrows or captures a provider, and the ordinary ownership and lifetime rules decide whether that capture is legal.

> **Dependency injection is not a magical runtime side channel. It participates in Silk’s ownership system.**

Provision is lexical rather than a mutation of a global container. It removes one exact service, role, and access requirement from one Effect layer. If that Effect later returns another Effect as ordinary data, the inner Effect is not silently provided too.

## Ownership you can read

Silk borrows—yes, the pun is unavoidable—from three places:

**Rust-style ownership and deterministic cleanup.**  
**Zig-style allocator explicitness.**  
**Effect-style dependency tracking.**

Values are either `Copy` or affine. User-defined structs are affine unless they explicitly request valid `Copy` conformance. Affine values may be borrowed or transferred, but not silently duplicated.

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

The vocabulary is intentionally visible:

```text
&message       borrow it
&mut message   borrow it exclusively
move message   give it away
```

After `move message`, the source owner is no longer available on that control-flow path. A shared borrow does not transfer ownership; `&mut` grants exclusive mutation access. Deterministic cleanup follows ownership.

Silk’s current rules are sometimes more restrictive than Rust’s. In particular, stage-0 Silk forbids partial moves out of aggregates that remain alive.

### The allocator is a service

Like Zig, Silk does not make allocation policy an invisible library decision. Unlike a conventional allocator parameter, however, allocator access participates in the same Effect requirement system as every other capability.

The standard-library service is equivalent in shape to:

```silk
service Allocator {
  effect fn allocate(layout: Layout)
    -> Allocation
    ! OutOfMemoryError
    ? &mut Allocator
}
```

That signature tells the whole story:

```text
returns       Allocation
can fail      OutOfMemoryError
requires      exclusive Allocator access
```

`SystemAllocator` is one concrete process-backed provider. Allocation refusal is an ordinary typed failure.

An allocating library operation can expose the same facts directly. `Box.make` currently has this shape:

```silk
effect fn make<T>(value: T)
  -> Box<T>
  ! OutOfMemoryError
  ? &mut Allocator
```

And the application chooses the allocation policy at the boundary:

```silk
import silk.allocator { Allocator }
import silk.box as Box
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

`Box.make` makes allocation visible, `provideMut` chooses the allocator, `run` produces one affine owner, and `Box.into(move boxed)` consumes that owner to transfer the contained value out.

`Box` itself is ordinary Silk source, not a compiler-recognized heap primitive. It coordinates destruction so its value and allocation are released exactly once.

There is one subtle architectural detail: a successful low-level `Allocation` is a self-contained affine owner. It does not retain a borrow of the allocator that created it. The allocator loan may end immediately after allocation, while the returned allocation moves independently with everything it needs for eventual release.

> **Ownership is syntax you can read. The allocator is a service.**

## Fibers: structured concurrency with ownership

Zig has spent years trying to make concurrency as honest as allocation. I watched Andrew Kelley painstakingly search for the right async and I/O abstraction. Naturally, because I am much smarter than him, [I stole mine from Effect](https://www.effect.website/docs/v4/concurrency/basic-concurrency).

Silk currently has a **local, cooperative, single-threaded structured-concurrency model** built from `Scheduler`, `Fiber`, `Execution`, and `Wake`.

A scheduler owns a root task and all of its descendants. Tasks run until they complete, park, or yield. `Fiber.forkChild` creates structured child work; `Fiber.await` and `Fiber.join` consume an affine authority to observe it.

At the center of the API:

```silk
import silk.allocator { OutOfMemoryError }
import silk.fiber as Fiber
import silk.scheduler as Scheduler

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
```

Three lines tell the story:

```text
forkChild(...)        create structured child work
Fiber<A, E>           receive an affine authority to observe it
join(move child)      consume that authority exactly once
```

A Fiber handle owns one completion observer. `await` returns `Fiber.Outcome<A, E>`, representing success, typed failure, or cancellation as ordinary data. `join` instead returns `A` on success and propagates the child’s `E` or `Fiber.Cancelled`.

Dropping the handle abandons observation, but it does not detach or independently cancel the child. The child remains structurally linked to the task that created it.

> **A Fiber is not a task ID. It is an owned capability to observe a task.**

That ties concurrency directly back into the memory model.

### The scheduler is explicit too

Silk does not silently choose a scheduler merely because an Effect can park. The application constructs and enters one explicitly:

```silk
let mut scheduler = LocalScheduler.make()

run LocalScheduler.execute(
  &mut scheduler,
  program(),
)
```

`LocalScheduler.execute` owns the root execution and drives it. The shipped scheduler uses deterministic FIFO readiness.

Even better, `Scheduler` itself is a service. In simplified form:

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

Alternative local scheduling policies can therefore be ordinary providers. The compiler supplies only the target-neutral mechanisms needed for execution, parking, waking, and lifecycle management. Fibers, queues, readiness policy, task storage, and scheduling live in ordinary Silk source.

> **Mechanism belongs to the runtime. Policy belongs in Silk.**

Structured concurrency keeps child lifetimes visible:

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

Parent success, failure, cancellation, or stalled shutdown causes unfinished descendants to be cancelled and released before the parent’s terminal outcome becomes observable. The initial API deliberately has no detached task, daemon task, or reparenting operation.

Creating a child may expose `OutOfMemoryError` and `Scheduler.TaskIdExhaustedError`. Joining may expose the child’s own `E` plus `Fiber.Cancelled`. Concurrency does not invent a separate exception universe; its recoverable operational failures remain ordinary Effect failures.

```text
forkChild
    ↓
Fiber<A, E>
    ↓
join
    ↓
A ! E | Cancelled
```

> **Concurrency does not get to escape the type system either.**

Not every invalid concurrency state is recoverable. Lifecycle violations and private execution-stack exhaustion remain fatal traps outside the Effect failure channel.

And one caveat matters: current `Execution`, `Wake`, and `Shared` values are local. Silk currently provides cooperative, single-threaded scheduling—not parallel, multithreaded fibers.

## What now?

Silk is not ready for production. It is ready to be experimented with, argued about, broken, and redesigned.

Its APIs may change freely. Networking, broad FFI, a package registry, self-hosting, and multithreaded scheduling are still ahead. The current compiler and standard library are enough to test the central idea, but not enough to pretend the language is finished.

That central idea is simple:

When a function can fail, the signature should say so.  
When it needs a capability, the type should say so.  
When it allocates, the allocator should be visible.  
When ownership moves, the syntax should show it.  
When concurrent work is created, its lifetime should have an owner.

Silk is not trying to make systems programming look as though nothing is happening.

> **It is trying to make everything that is happening fit together—and make the program tell the truth about it.**
