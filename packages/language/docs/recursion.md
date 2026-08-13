# Recursion and the machine stack

Silk gives ordinary recursion **no bounded-stack guarantee**. A call costs a machine call frame, so
a recursive function is limited by the stack of whatever is running it, and a recursion deep enough
to run out of stack fails the way that engine fails — abruptly, and differently on each one.

This page states that boundary, shows what hitting it looks like on each engine, and gives the
pattern to use instead when a structure can be arbitrarily deep.

## 1. The guarantee, stated plainly

- An ordinary call is an ordinary call. It consumes a frame on the machine or host stack and
  releases it on return.
- **Recursion depth is bounded by the target's stack**, which is a property of the host, the profile,
  and the frame the optimizer chose — not of Silk. The bound is not a number the language promises.
- The compiler **does not** add a shadow stack, a per-call heap allocation, or a trampoline to make
  deep recursion survivable.
- A traversal that must handle unbounded depth is **written iteratively**. That is the supported
  answer, and it is a source-level answer, not a compiler feature.

### Why the compiler stays out of it

The rule is pay-for-use. Making arbitrary recursion stack-safe means spilling frames somewhere other
than the stack, and there is no version of that which only the deep call sites pay for:

- **Every call would pay.** The overwhelming majority of calls in a program are shallow. A shadow
  stack taxes all of them to protect the few that are not, and it does so in the hottest thing a
  compiler emits.
- **Every call would need an allocator.** Spilled frames live somewhere, and in Silk there is no
  ambient heap — allocation is an `Allocator` capability that a function has to require, and it can
  fail with `OutOfMemory`. Hidden per-call spilling would give an allocator requirement and a
  failure row to functions that declare neither.
- **Some calls cannot have one.** A `Drop` hook may declare no failure row and no capability
  requirement at all, so there is no legal place to put a hidden allocation inside cleanup. A
  guarantee the language cannot extend to cleanup is not a guarantee the language can state.

So the machine stack stays the machine stack, and depth stays the program's problem to structure.

## 2. The same limit has three faces

A deep recursion does not fail the same way twice, and a reader who has only seen one of these will
not recognise the others. They are one boundary:

| Engine | What you see | Recoverable? |
| --- | --- | --- |
| **Bootstrap evaluator** | A `Blocked` outcome whose reason is `EvaluationLimit` with kind `CallDepth`, naming the recursive function, the span, and the whole active call path | Yes — it is a value, not a crash |
| **Native** | The process dies on a signal: `SIGSEGV` (the stack guard page), no exit status, no diagnostic, no unwinding, no partial output | No |
| **WebAssembly** | The host throws out of the exported call — under V8, `RangeError: Maximum call stack size exceeded` | No, but the host survives |

The evaluator is deliberately the odd one out. Its depth limit is a deterministic budget rather than
a real stack, so the same program blocks at the same place on every machine instead of crashing on
some of them. That is why evaluating a program successfully does not prove the compiled program has
the stack for it, and why a `CallDepth` block is worth reading as "this recursion is deep", not as
"the evaluator is small".

On WebAssembly the specific trap is **platform-sensitive**: a band of depths just below the host
limit may trap `unreachable` (`WebAssembly.RuntimeError`) rather than raise the host's `RangeError`.
Either way the meaning is the same — the engine could not carry the chain.

## 3. Measured depths, for calibration only

These are observations on one machine, at one moment, at one optimization level. They move with the
host's stack size, the profile, and the frame layout the optimizer picks, and **nothing in the
compiler promises them**. They are here so the numbers feel concrete, not so you can rely on one.

Recorded on x86_64 Linux, Node 22.22, clang 18, release profile, walking a chain of boxed nodes one
level per recursive call:

| Engine | Deepest chain that survived | Shallowest that failed |
| --- | --- | --- |
| Bootstrap evaluator | 500 | 510 (the 1,024-frame `CallDepth` budget) |
| WebAssembly (Node/V8) | 950 | 1,000 |
| Native (release) | 100,000 | 128,000 |

The three differ by more than two orders of magnitude, which is the practical point: a depth that is
comfortable natively is fatal in a browser, and a depth the evaluator accepts proves nothing about
either.

## 4. The iterative pattern

The chain below is built, walked, and destroyed without a single recursive call. Two things carry
the weight:

- **`while`** is the loop, and
- **`Intrinsic.replace`** is what lets ownership walk down the chain. A local cannot be partially
  moved out of, and a `match` arm is an expression rather than a statement, so a loop that takes the
  next link out of the value it holds has to swap a sentinel into the place it took from.

```silk
import silk.box { Box, make as boxMake, into as boxInto }

pub struct End {}

pub struct Link {
  next: Box<Chain>
}

pub struct Step {
  kind: End | Link
}

pub struct Chain {
  step: Step
}

// One unlink step, carrying back both the next level and whether there was one. A match arm cannot
// assign, so the loop below needs the answer as a value rather than as a side effect.
pub struct Unlinked {
  chain: Chain
  more: bool
}

fn unlink(chain: Chain) -> Unlinked {
  return match move chain {
    Chain { step } => unlinkStep(move step)
  }
}

fn unlinkStep(step: Step) -> Unlinked {
  return match move step {
    Step { kind } => unlinkKind(move kind)
  }
}

fn unlinkKind(kind: End | Link) -> Unlinked {
  return match move kind {
    End nothing => Unlinked { chain: Chain { step: Step { kind: End {} } }, more: false }
    Link { next } => Unlinked { chain: boxInto<Chain>(move next), more: true }
  }
}

// Builds outward instead of downward: each turn wraps the chain built so far in one more link.
effect fn build(depth: i32) -> Chain ! OutOfMemory ? &mut Allocator {
  let mut current = Chain { step: Step { kind: End {} } }
  let mut remaining = depth
  while remaining > 0 {
    let taken = Intrinsic.replace(current, Chain { step: Step { kind: End {} } })
    let boxed = run boxMake<Chain>(move taken)
    current = Chain { step: Step { kind: Link { next: move boxed } } }
    remaining = remaining - 1
  }
  return move current
}

// Counts the links by walking one level per loop turn. However long the chain is, this function's
// own stack is one frame deep.
fn length(chain: Chain) -> i32 {
  let mut current = move chain
  let mut counted = 0
  let mut going = true
  while going {
    let taken = Intrinsic.replace(current, Chain { step: Step { kind: End {} } })
    let mut stepped = unlink(move taken)
    current = Intrinsic.replace(stepped.chain, Chain { step: Step { kind: End {} } })
    going = stepped.more
    if stepped.more { counted = counted + 1 }
  }
  return counted
}

effect fn measure() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let built = run build(4096) |> Effect.provideMut(&mut allocator)
  let counted = length(move built)
  if counted == 4096 { return 0 }
  return 2
}

effect fn recover(error: OutOfMemory) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catch(measure(), recover) }
```

The recursive spelling of `length` is shorter and reads better, and for a chain of known-small depth
it is the right code. The version above is what you reach for when the depth comes from input.

For comparison, this is the recursive walk that the boundary applies to — correct, idiomatic, and
bounded by the stack:

```silk
import silk.box { Box, make as boxMake, get as boxGet }

pub struct End {}

pub struct Link {
  next: Box<Chain>
}

pub struct Step {
  kind: End | Link
}

pub struct Chain {
  step: Step
}

// One frame per level, plus one for the view. Fine at 64 levels; fatal at a million.
fn stepDepth(step: &Step) -> i32 {
  return match &step.kind {
    End nothing => 0
    Link { next } => viewDepth(boxGet<Chain>(&next))
  }
}

fn viewDepth(view: &[Chain]) -> i32 {
  return match &view[usize.ZERO] {
    Chain { step } => 1 + stepDepth(&step)
  }
}

effect fn build(depth: i32) -> Chain ! OutOfMemory ? &mut Allocator {
  let mut current = Chain { step: Step { kind: End {} } }
  let mut remaining = depth
  while remaining > 0 {
    let taken = Intrinsic.replace(current, Chain { step: Step { kind: End {} } })
    let boxed = run boxMake<Chain>(move taken)
    current = Chain { step: Step { kind: Link { next: move boxed } } }
    remaining = remaining - 1
  }
  return move current
}

effect fn measure() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let built = run build(64) |> Effect.provideMut(&mut allocator)
  let counted = stepDepth(&built.step)
  drop built
  if counted == 64 { return 0 }
  return 2
}

effect fn recover(error: OutOfMemory) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catch(measure(), recover) }
```

## 5. What about effects?

Wrapping the recursion in `effect fn` does not change any of this. An effect that is `run` at the
call site is an ordinary call and costs an ordinary frame; the effect system describes what a
computation may do, not where its frames live.

A recursion that explicitly crosses a suspension boundary — where the continuation is reified rather
than left on the stack — is the other shape that can be deep by construction. That boundary is being
designed with the coroutine work and **is not available today**. Until it is, iterate.

## See also

- [Language reference](./reference.md) — `while`, `match`, and the ownership rules the loops above
  are working within.
- [Standard library](./stdlib.md) — `silk.box` and the rest of the modules.
