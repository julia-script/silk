## Context

Two questions had to be answered before this combinator could be written, and both were answered
by decision rather than derived: what happens when the finalizer fails, and where the finalizer sits
relative to the protected Effect's own local cleanup. This document records why the chosen answers
are the ones the existing machinery already supports.

## Goals / Non-Goals

Goals:

- One place to put cleanup that must run whether the Effect succeeded or failed.
- An order between that cleanup and the Effect's own locals that is a consequence of how the
  combinator is written, not a comment asserting it.
- Composition with a release that can itself fail, without the combinator having to reconcile two
  outcomes.

Non-Goals: a fallible finalizer, interruption, trap recovery, running the finalizer on a trap, and
any change to `Effect.catch`, `Effect.result`, or the propagation exit.

## Decisions

### The finalizer is `! never`

The alternative is a finalizer that can fail, which forces the combinator to decide what happens
when both the protected Effect and the finalizer produce a failure. Every answer to that is a
policy: drop one, prefer one, or widen the failure type to carry both. The first two lose
information silently, and the third makes `ensuring` change the failure type of everything it wraps —
directly contradicting the requirement that it hand on the original failure unchanged.

Typing the finalizer `once Effect<() ! never ? S>` removes the question instead of answering it.
There is no second outcome, so there is nothing to reconcile, and the return type stays `A ! E ? R | S`
— the protected Effect's own failure type, widened only in its requirement row by whatever the
finalizer requires. This matches the existing rule that a `Drop` hook carries no failures: cleanup
that is guaranteed to run is cleanup that cannot fail.

A fallible release is not thereby excluded; it is relocated. The caller recovers it first and the
recovered Effect is what gets passed:

```silk
effect fn ignore(error: OutOfMemoryError) -> () { return () }

effect fn finalize() -> () {
  return run Effect.catch(release(), ignore)
}
```

The decision about what a failed release means stays with the caller, who is the only party with
enough context to make it. This is the shape a scoped resource wrapper uses: `make` composed with
`ensuring(release)`, where `release` may be fallible and the wrapper chooses its own recovery.

### Local cleanup runs first, then the finalizer

Locals are cleaned in reverse acquisition order. The finalizer is acquired outside the protected
Effect — `ensuring` wraps it — so reverse order puts the finalizer last. Any other choice would make
the finalizer observe the Effect's locals *after* it had committed to running, which is both harder
to reason about and impossible to guarantee, since those locals belong to a frame the combinator
does not own.

### The order is produced by reification, not asserted

`ensuring` could have been written to run the finalizer on the propagation path. That is the shape
that leaks: a typed failure propagating out of the protected Effect leaves its frame behind, and any
owner still live at that run is stranded unless a propagation exit releases it. `provideWith` hit
exactly this and was restructured around a reified `Result` for the same reason.

So `ensuring` reifies first:

```silk
pub effect fn ensuring<A, E, ?R, ?S>(
  self: once Effect<A ! E ? R>,
  finalizer: once Effect<() ! never ? S>
) -> A ! E ? R | S {
  let completed = run result(move self)
  let finalized = run move finalizer
  return match move completed {
    Result<A, E> { value: outcome } => match move outcome {
      Success<A> { value: success } => move success
      Failure<E> { error } => run raise(move error)
    }
  }
}
```

The typed failure arrives as data. The protected Effect's frame has already exited and its locals
are already cleaned by the time `run move finalizer` starts, so the order is a property of the
control flow rather than a claim about it. The outcome is re-raised only after the finalizer has
returned, so a recovering caller never observes the outcome before the finalizer has run.

Two ownership facts make this safe rather than merely convenient. The finalizer's own run is
infallible — `! never` means an empty failure type — so it publishes no
propagation exit and cannot strand `completed`, which is live across it. And the owner the protected
body acquired is released by that body's own exit, not by this frame: the propagation exit excludes
a run's own consumed operands, so an owner moved in through `ensuring`'s operands is the callee's
and is not released twice here.

### A trap is not an outcome

Traps bypass `Effect.catch` and bypass `Drop`. The finalizer is not an exception to that, and
nothing in this change makes it one: a trap does not produce a `Result`, so `run result(move self)`
never returns and the finalizer is never reached. The bypass is structural, not a special case.

## Risks / Trade-offs

The reification costs one `Result` materialization per protected Effect on both paths, including
the successful one. This is the same cost `map`, `flatMap`, `tap`, `catch`, and `provideWith`
already pay, and the alternative — running the finalizer on the propagation path — is the shape that
leaks, so the trade is not actually open.

`? S` widens the requirement row with whatever the finalizer requires. That is visible in the type
and discharged by the ordinary row rules; no requirement is hidden.
