# Effects and execution

An Effect is a lazy computation. Calling an effect function constructs an Effect; `run` executes
exactly one Effect layer. An Effect may succeed with another Effect as an ordinary nested value.

## EFF-001 — Calling an effect function constructs an Effect

**Status:** Confirmed

An effect function declared with success type `A`, failure type `E`, and requirement row `R` has call
result `Effect<A ! E ? R>`. Calling it does not execute its body.

```silk
effect fn answer() -> i32 { return 42 }

pub fn main() -> i32 {
  let pending = answer()
  return run pending
}
```

**Boundary:** An ordinary function call executes immediately and returns its declared result rather
than constructing an Effect.

**Diagnostics:** Calling an effect function is not itself an error. Using its `Effect<A>` result in
a context that requires `A` must produce a type-mismatch diagnostic naming both types at the use
site. Returning it from a body whose contract requires `A` produces `SEM0129`. Ignoring the Effect
as an expression statement is the separate `SEM0087` boundary described under
[statements and discarded values](statements-and-discarding.md).

**Evidence:** [effect-contract decision](../../wayfinder/bootstrap-language/issues/03-function-contracts-services-and-failures.md),
[flow specification](../../openspec/specs/bootstrap-flow-functions/spec.md).

## EFF-002 — An effect body returns its success value

**Status:** Confirmed

Inside `effect {}` or an effect function body, `return` accepts the Effect's success value. It does
not implicitly execute or flatten an Effect expression.

```silk
effect fn inner() -> i32 { return 42 }

effect fn outer() -> i32 {
  return run inner()
}

pub fn main() -> i32 { return run outer() }
```

**Boundary:** The following body attempts to return `Effect<i32>` where its success type is `i32` and
must receive a source diagnostic:

```silk,ignore
effect fn inner() -> i32 { return 42 }
effect fn outer() -> i32 { return inner() }
```

**Diagnostics:** Returning a value incompatible with an effect body's declared success type must
produce a return-type mismatch at the returned expression, naming the declared success type and the
actual expression type. `SEM0129` reports an incompatible explicit return at the returned
expression. `SEM0130` reports reachable fallthrough from a body whose declared result is not `()`.
HIR may retain explicitly unavailable structure for inspection, but realization, layout, MIR, and
backends are unavailable while either diagnostic exists.

This is not intentional tail propagation. Issue
[#226](https://github.com/julia-script/silk/issues/226) records the interface-dispatched version of
the former source/backend disagreement and is now a semantic regression case.

The opposite direction is equally invalid:

```silk,ignore
effect fn inner() -> i32 { return 42 }

fn outer() -> Effect<i32> {
  return run inner()
}

pub fn main() -> i32 {
  return run outer()
}
```

Here `run inner()` is `i32`, which cannot satisfy `outer`'s declared `Effect<i32>` result. Analysis
reports `SEM0129`; it does not construct MIR or an executable program.

Both valid alternatives make the boundary explicit:

```silk
effect fn inner() -> i32 { return 42 }

fn deferred() -> Effect<i32> { return inner() }
fn executed() -> i32 { return run inner() }
```

**Evidence:** [explicit execution syntax](../../wayfinder/bootstrap-language/issues/08-prototype-bootstrap-syntax.md),
[effect elaboration](../../packages/compiler/src/Elaboration.ts),
[return-contract regressions](../../packages/compiler/test/InterfaceBounds.test.ts),
[MIR return verification](../../packages/compiler/src/Mir.ts).

## EFF-003 — `run` executes exactly one Effect layer

**Status:** Confirmed

For `pending: Effect<A ! E ? R>`, `run pending` produces `A` while propagating unhandled failures and
requirements through the surrounding Effect execution boundary. One `run` never removes two nested
Effect layers.

```silk
effect fn inner() -> i32 { return 42 }
effect fn outer() -> Effect<i32> { return inner() }

pub fn main() -> i32 {
  return run run outer()
}
```

**Boundary:** Running `outer()` once produces `Effect<i32>`, not `i32`.

The following invalid case does not demonstrate automatic flattening:

```silk,ignore
effect fn inner() -> i32 { return 42 }
effect fn outer() -> i32 { return inner() }

pub effect fn main() {
  let value = run outer()
}
```

`outer()` has type `Effect<i32>` from its declaration, so one `run` would give `value` type `i32`.
The program is nevertheless invalid because `outer` returns `Effect<i32>` where its success
contract requires `i32`; `SEM0129` rejects the declaration before lowering.

**Diagnostics:** Applying `run` to a non-Effect value reports `SEM0065` at the operand and identifies
its actual type. A `run` that leaves failures or requirements outside the surrounding contract
reports `SEM0066` or `SEM0071`, respectively. If one `run` validly produces another Effect but its
context requires the nested success value, the required error is the type mismatch described under
EFF-002; `run` must not silently remove the second layer.

**Evidence:** [one-layer execution test](../../packages/compiler/test/Elaboration.test.ts),
[Effect runtime tests](../../packages/compiler/test/EffectRuntime.test.ts).

## EFF-004 — Nested Effects are ordinary values

**Status:** Confirmed

An Effect may succeed with another Effect. The resulting `Effect<Effect<A>>` remains nested until
source executes each layer or applies an explicit flattening operation.

```silk
effect fn inner() -> i32 { return 42 }
effect fn nested() -> Effect<i32> { return inner() }
```

**Boundary:** Declaring the success type as `i32` does not request implicit flattening. Returning an
`Effect<i32>` from that body produces `SEM0129` as recorded under EFF-002.

**Diagnostics:** Constructing or returning a nested Effect is valid and produces no diagnostic.
Using `Effect<A>` where `A` is required must produce the type mismatch described under EFF-002.

**Evidence:** [Effect flattening runtime tests](../../packages/compiler/test/EffectRuntime.test.ts).

## EFF-005 — An ordinary function can construct a deferred Effect

**Status:** Confirmed

Calling an ordinary function executes its body immediately. Evaluating an `effect {}` expression
constructs an Effect but does not execute the statements inside the block. Values computed by the
ordinary function may therefore be used later by the deferred block.

```silk
fn prepareAnswer() -> Effect<i32> {
  let n = 123 // executes when prepareAnswer() is called
  return effect {
    return n // executes when the returned Effect is run
  }
}

pub fn main() -> i32 {
  return run prepareAnswer()
}
```

Evaluation of `run prepareAnswer()` has two ordered steps:

1. Call `prepareAnswer()`, compute `n`, and construct the returned `Effect<i32>`.
2. Run that Effect, which executes the `effect` block and returns `n`.

**Boundary:** Calling `prepareAnswer()` without `run` still computes `n` and constructs the Effect;
it does not execute `return n` inside the `effect` block. This rule describes execution timing only.
Values retained by the deferred block follow the ordinary
[capture rules](ownership-and-borrowing.md#capture-001--delayed-values-acquire-their-captures-when-constructed).

**Diagnostics:** This evaluation order is valid behavior, not a source restriction, so it has no
diagnostic. Invalid captures receive the corresponding capture, borrow, move, or escape diagnostic.

**Evidence:** [function-contract decision](../../wayfinder/bootstrap-language/issues/03-function-contracts-services-and-failures.md),
[effect-block elaboration](../../packages/compiler/src/Elaboration.ts),
[effect-block lowering](../../packages/compiler/src/Lower.ts).

## EFF-006 — An ordinary function may run only a closed Effect

**Status:** Confirmed

An ordinary `fn` has no typed-failure or requirement channels. The Effect at a `run` site inside an
ordinary function must therefore have failure type `never` and an empty requirement row. All typed
failures must be recovered or reified as values, and all requirements must be provided, before
execution crosses that boundary.

The rule applies to the final residual channels after composition. A recovery operation whose
handler can fail, or a provision operation whose acquisition has requirements, does not close the
boundary unless those new channels are also eliminated.

```silk
import silk.effect as Effect

struct ProblemError {}

effect fn risky() -> i32 ! ProblemError {
  fail ProblemError {}
}

effect fn recover(error: ProblemError) -> i32 {
  return 0
}

pub fn main() -> i32 {
  return run Effect.catch<ProblemError>(risky(), recover)
}
```

An `effect fn` may instead propagate residual failures and requirements when its own contract
contains them.

An executable `effect fn main` is itself an Effect boundary handled by the generated program entry
adapter; it is not an ordinary-function exception to this rule. See
[program entry](program-entry.md).

**Boundary:** Neither a non-`never` failure type nor a nonempty requirement row may escape through an
ordinary function's `run`.

```silk,ignore
struct ProblemError {}

effect fn risky() -> i32 ! ProblemError {
  fail ProblemError {}
}

pub fn main() -> i32 {
  return run risky()
}
```

```silk,ignore
service Counter {
  effect fn get() -> i32 ? &Counter
}

effect fn read() -> i32 ? &Counter {
  return run Counter.get()
}

pub fn main() -> i32 {
  return run read()
}
```

**Diagnostics:** A residual failure type other than `never` reports `SEM0066` at `run` and names every
unhandled failure member. A nonempty residual requirement row reports `SEM0071` at `run` and names
every unsatisfied requirement member. When both channels remain open, the diagnostics must expose
both rather than stopping after the first category.

**Evidence:** [effect-contract decision](../../wayfinder/bootstrap-language/issues/03-function-contracts-services-and-failures.md),
[flow specification](../../openspec/specs/bootstrap-flow-functions/spec.md),
[run diagnostics](../../packages/compiler/test/Elaboration.test.ts).

## Related Effect rules

The confirmed rules above define construction, one-layer execution, nested success values, the
eager/deferred boundary, and closure at an ordinary execution boundary. The rest of the current
language-level Effect model is defined in focused pages:

| Area | Reference |
| --- | --- |
| Success, failure, and requirement channels | [Effect contracts](effect-contracts.md) |
| Requirements, services, roles, and provision | [Requirements and services](requirements-and-services.md) |
| Captures, run access, reuse, and cleanup | [Ownership and borrowing](ownership-and-borrowing.md) |
| Typed propagation, recovery, and traps | [Typed failures](typed-failures.md) |
| Stack-safe recursive transfer | [Effect suspension and stack-safe recursion](effect-suspension.md) |
| Cancellation, interruption, concurrency, and async cleanup | Not part of the stabilized language |

The standard-library operations built from these rules—including `of`, `result`, `mapBoth`, `map`,
`mapError`, `flatMap`, `flatten`, `zip`, `zip3`, `tap`, `catch`, `catchAll`, `ensuring`,
`ifThenElse`, `retry`, the requirement-binding and provision operations, and `suspend`—belong in an
Effect API reference rather than on this language-semantics page.

The signature-level rules for Effect channels, declaration bounds, generic failure types and
requirement rows, and concrete identity are defined in [Effect contracts](effect-contracts.md).
Valid failure values, propagation, recovery, cleanup, diagnostic context, and fatal traps are
defined in [typed failures](typed-failures.md).
