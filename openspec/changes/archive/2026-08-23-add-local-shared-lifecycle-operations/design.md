## Context

The prerequisite slices provide an affine local core initialized at count one with available access
and private reclaim authority. This design gives that core a closed single-threaded lifecycle. See
`proposal.md` and the ownership and intrinsic delta specs.

## Goals / Non-Goals

**Goals:**

- Make clone infallible except for a pre-mutation fatal overflow trap.
- Confine one exclusive access to exactly one ordinary callback invocation.
- Clean `T` and release storage exactly once at the last handle.

**Non-Goals:**

- Add overlapping readers, Effect callbacks, recoverable clone failure, Weak, atomics, or cycle collection.
- Promise restoration or cleanup after a fatal trap.

## Decisions

### Use independent strong-count and access-state machines

The logical control block has `strong: 1...MAX` and `access: Available | Active`. Strong operations
do not inspect or change access except that the last drop is unreachable while a borrowed receiver
keeps the active call's handle live. Access operations do not inspect or change the count.

`sharedClone` first compares `strong` with the target's maximum. At the maximum it traps without a
store or result. Otherwise it increments once and returns one new non-Copy handle; it never touches
`T`, allocates, or gains a failure channel. Dropping a non-last handle decrements once. Dropping the
last transitions to terminal ownership, cleans `T`, then consumes the retained reclaim authority.

The maximum is a compiler-planned fact of the selected target's count representation. Evaluation and
the emitting backend for that same selected target consume the identical maximum and therefore agree
on the success-or-trap boundary; different target representations need not share one numeric maximum.

Alternatives rejected: saturating or wrapping counts manufacture untracked obligations; coupling
count to access would prohibit harmless clone/non-last drop in callbacks; returning overflow data
would change the accepted infallible clone API.

### Make access callback-shaped and all-exclusive

`sharedWithMut(self, use, onConflict)` checks `access` once. If active, it invokes only
`onConflict` while leaving the active state untouched. If available, it marks active, creates one
position-restricted exclusive borrow of `T`, invokes only `use`, ends the borrow, restores available,
and returns that callback's result. The operation allocates nothing. Both callbacks are take-once
ordinary callables with a common result type. After the selected callback returns normally, the
unselected callback environment is discharged exactly once through ordinary callable cleanup; on
the successful path, the restricted borrow ends and access is restored before that cleanup and the
result reach the caller. A fatal trap retains Silk's general no-unwind semantics.

There is no reader count and no second read primitive. Public shared inspection can later narrow the
exclusive borrow in ordinary source. The four nested public access combinations therefore select
conflict and never form a second reference.

Alternatives rejected: a take-and-replace transaction imposes policy and whole-value movement; a
separate read state is unearned by the driving examples; returning a compiler-known conflict error
would privilege public policy.

### Reuse recursive position-restricted-borrow checking

The `use` parameter borrow is rooted in the control block and valid only for that invocation. The
result type and every generic substitution are checked recursively for the restricted borrow.
Capturing it into an Effect, callable, owned aggregate, or failure value is rejected, as is any path
that suspends while it remains live. Diagnostics identify both the escape/suspension site and the
access boundary that created the borrow.

### Represent cleanup as one opaque-core action

Ownership plans retain one whole-handle cleanup action rather than recursively planning `T` per
handle. Runtime count selection decides whether it is a decrement or the unique last cleanup. The
last action invokes `T`'s canonical cleanup exactly once before release. Strong cycles intentionally
never reach that action and leak. An acyclic graph whose handles are all discharged by structured
execution must balance acquisition and release; a fatal trap retains the repository's no-unwind
contract and makes no cleanup promise.

## Risks / Trade-offs

- **Risk: result typing hides an escaping borrow behind a generic** → check the specialized result
  recursively and retain the access-boundary provenance in the diagnostic.
- **Risk: cleanup order diverges between explicit and structured drop** → lower both to the same
  verified opaque-core cleanup action.
- **Risk: overflow is impractical to exercise at native width** → unit-test the transition against a
  reduced private maximum and structurally verify the emitted compare-before-store ordering.
