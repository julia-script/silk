## Why

Silk has no combinator that runs a finalizer after an Effect whatever the outcome. `Effect.catch`
recovers a typed failure but does not run on success, so a caller who needs cleanup on both paths
writes the cleanup call twice — once after the success and once in each recovery arm. There is no
single place to put it, and the closed combinator list in `bootstrap-flow-functions` omits
`ensuring` without anything rejecting it.

This is not a pure library addition, because a finalizer that must run on both outcomes has to have
a defined position against two mechanisms that already exist:

- **Local cleanup.** An Effect's own locals are cleaned in reverse acquisition order when its frame
  exits. A finalizer needs a defined order against that cleanup, and the specification does not
  have one to point at.
- **The propagation exit.** A typed failure that propagates out of a frame leaves that frame behind.
  A combinator that runs code _after_ a failure therefore has to decide whether the failure reaches
  it as a propagation or as data — and getting that wrong is exactly the leak `bootstrap-ownership`
  had to fix for a generic body holding an owner across a fallible run.

## What Changes

- Add `Effect.ensuring(self, finalizer)` to canonical standard-library source. It runs the finalizer
  after the protected Effect completes and hands on the original success value or the original typed
  failure unchanged.
- Type the finalizer `once Effect<() ! never ? S>`. A finalizer failure is unrepresentable by
  construction rather than handled, consistent with the existing rule that `Drop` hooks carry no
  failures. A caller with a fallible release recovers it into `! never` first — with `Effect.catch`,
  for example — and decides there what a failed release means.
- Fix the order: the protected Effect's own local cleanup runs first, then the finalizer. The
  finalizer wraps the Effect, so it exits last, which is the reverse-acquisition rule already
  governing locals applied one level out.
- Keep traps outside. A trap bypasses the finalizer exactly as it bypasses `Effect.catch` and every
  `Drop` hook.
- Add `ensuring` to the closed list of combinators that resolve to ordinary Silk declarations, so no
  compiler-side name recognition is introduced.

The implementation is ordinary Silk. `ensuring` reifies the protected Effect with `Effect.result`
before running the finalizer, which is what produces the order rather than merely documenting it:
reification means the protected Effect's frame — and every local it cleans up — is already gone when
the finalizer starts, and the outcome is re-raised only afterwards.

## Capabilities

### Modified Capabilities

- `bootstrap-flow-functions`: specify the finalizer combinator — that it runs on both outcomes,
  preserves the outcome it observed, carries an infallible finalizer, orders that finalizer after
  the protected Effect's local cleanup, and stays outside the trap path — and add `ensuring` to the
  closed list of library-defined combinators.

## Impact

The change affects canonical standard-library source (`silk/effect`), the compiler-shipped source
table generated from it, the generated standard-library documentation page, and acceptance tests. It
adds no intrinsic, no HIR or MIR operation, no diagnostic code, and no compiler-side name
recognition.

It introduces no fallible finalizer, no interruption (Silk has none), and no trap recovery. It does
not change `Effect.catch`, `Effect.result`, `provideEffect`, or the propagation exit.

`Effect.ensuring` is the cleanup mechanism a scoped temporary directory is specified to use — `make`
composed with `ensuring(release)` rather than a `Drop` hook — which is why the fallible-release
composition above is part of the contract rather than left to the caller to discover.
