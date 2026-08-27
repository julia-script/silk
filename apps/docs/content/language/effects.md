# Effects, failures, and services

An Effect is a lazy computation whose complete public contract is visible in its type. The type
tracks what the computation returns, how it can fail, and which runtime capabilities it needs.

```text
Effect<A ! E ? R>
       │   │   └─ requirements
       │   └───── typed failure
       └───────── success
```

The channels are independent. A computation can have a requirement without a failure, a failure
without a requirement, both, or neither.

## Construction is not execution

Calling an `effect fn` constructs an Effect value. It does not enter the function body. `run`
executes exactly one Effect layer:

```silk
effect fn answer() -> i32 {
  return 42
}

pub fn main() -> i32 {
  let pending = answer()
  return run pending
}
```

Nested Effects remain nested. If one Effect succeeds with another `Effect<i32>`, one `run` produces
that inner Effect rather than silently flattening it. Use a second `run` or the source-defined
`Effect.flatten` operation when flattening is intentional.

An ordinary `fn` can run only a closed Effect: its remaining failure type must be `never` and its
requirement row must be empty. An `effect fn` may propagate both channels when its own declaration
includes them.

## Typed failures are values

`fail value` stops the current Effect with an ordinary owned value. The value's type appears after
`!` in the function contract. It needs no marker interface and can be matched, stored, transformed,
or recovered like other data.

`Effect.catchAll` recovers the complete failure type. `Effect.catch<Selected>` recovers selected
members and preserves the residual failure union. `Effect.result` reifies success or failure as
ordinary `Result` data.

Traps are not typed failures. Division by zero, bounds violations, trapping overflow, violated
unsafe contracts, and exhausted execution storage bypass `catch`, `catchAll`, `result`,
`Effect.ensuring`, and `Drop`. Use a checked operation that returns data or a typed failure when the
caller must recover.

## Requirements are capabilities

A `service` declares behavior supplied at runtime. Calling one of its operations adds an entry to
the Effect requirement row:

```silk
import silk.effect { Effect }

pub struct TooLateError {}

service Clock {
  effect fn value() -> i32 ? &Clock
}

struct FixedClock {
  value: i32
}

impl Clock for FixedClock {
  effect fn value(self: &Self) -> i32 {
    return self.value
  }
}

effect fn readBefore(limit: i32) -> i32 ! TooLateError ? &Clock {
  let value = run Clock.value()
  if value > limit {
    fail TooLateError {}
  }
  return value
}

effect fn recover(error: TooLateError) -> i32 {
  return 0
}

pub fn main() -> i32 {
  let clock = FixedClock { value: 42 }
  let provided = Effect.provide(readBefore(50), &clock)
  return run Effect.catchAll(provided, recover)
}
```

`readBefore` tells callers everything they need to know: success is `i32`, failure is
`TooLateError`, and execution needs shared `Clock` access. `Effect.provide` lends the provider for
that lexical computation and removes exactly the selected requirement. It does not mutate a global
registry.

Provider modes preserve access:

| Operation | Provider access | Typical use |
| --- | --- | --- |
| `Effect.provide` | shared borrow | read-only services |
| `Effect.provideMut` | exclusive borrow | allocators, streams, filesystems, stateful test doubles |
| `Effect.bindRequirementOwned` | ownership transfer | a provider retained by the computation |
| `Effect.provideEffect` | acquired by an Effect | fresh scoped provider per execution or retry |

Roles distinguish multiple requirements for the same service. If one provider could satisfy more
than one row member, select the complete member explicitly as the first generic argument.

## Services and interfaces solve different problems

A `service` is a runtime Effect contract. Its operations create requirement-row entries, and a
value with a matching `impl` supplies them lexically.

An `interface` is a compile-time conformance contract. It constrains generic specialization and
does not create a runtime service slot or dispatch table. The compiler specializes a finite set of
concrete implementations; interface names never become ambient requirements.

Use a service when application wiring or tests must replace behavior at runtime. Use an interface
when a generic algorithm needs compile-time evidence that a concrete type supports an operation.

## Composition stays lazy

The standard `silk.effect` module is ordinary Silk source. Its operations do not receive special
treatment from the compiler:

- `map` transforms success;
- `mapError` transforms typed failure;
- `flatMap` continues with another Effect;
- `zip` and `zip3` run Effects sequentially and collect their results;
- `tap` runs a dependent Effect while preserving the original success;
- `catch` and `catchAll` recover failures;
- `retry` reruns a reusable Effect;
- `ensuring` runs an infallible finalizer after success or typed failure;
- `provide`, `provideMut`, and `provideEffect` satisfy requirements.

Direct `run` is usually clearest for straight-line code. Pipelines are useful when the operations
transform one Effect as data. Composition unions the failure and requirement channels, and typed
failure stops sequential work before later steps run.

## Suspension is explicit stack safety

`Effect.suspend(child)` transfers one deferred child through compiler-owned execution machinery.
It preserves the child's three channels exactly and lets a recursive Effect cycle use bounded
native and WebAssembly machine stack when every cycle crosses a suspension boundary.

Suspension does not park, yield, or select a scheduler. Cooperative task scheduling uses the
separate `Execution`, `Scheduler`, and `Fiber` APIs. Ordinary recursion, an uncovered Effect cycle,
and recursive cleanup still use the machine stack.

## Entry points

Executable programs use one of two forms:

```text
pub fn main() -> i32
pub effect fn main() -> () ! E
```

An ordinary entry returns the process status and can run only a closed Effect. An effectful entry
must have no remaining requirements; success is status 0, and an unhandled typed failure is
reported with status 1 after its payload is cleaned up.

## See also

- [Getting started](./tutorial.md#effects-failure-in-the-type)
- [Language reference: effects and execution](../reference/effects-and-execution.md)
- [Ownership, borrowing, and cleanup](./ownership.md)
- [Fibers and local scheduling](./fibers.md)
- [Recursion and stack safety](./recursion.md)
- [Standard library: Effect](./stdlib/effect.md)
- [Standard library: services and providers](./stdlib/)
