# Define effect contracts, services, and failure propagation

Type: grilling
Status: resolved

## Question

How do eager functions, lazy effects, failure rows, requirement rows, nominal service contracts,
provision, propagation, handling, and compile-time elaboration compose without runtime dependency-tag
lookup or a universal scope primitive?

## Answer

Ordinary `fn` functions execute directly. They may perform eager pure work and construct an effect
value, but effectful operations themselves execute only inside an effect. An `effect { ... }`
expression creates a lazy typed `Effect<A ! E ? R>` whose imperative body does not execute during
construction. An `effect fn` is sugar for an ordinary function whose complete body is such a lazy
effect. `run` evaluates exactly one effect layer: success produces `A`, an unhandled typed failure
from `E` aborts that execution and propagates, and unsatisfied service requirements remain in `R`.

The explicit expression form preserves the eager/lazy boundary across function bodies:

```silk
fn risky<T>(value: T, selector: i32) -> Effect<T ! Problem> {
  let prepared = prepare(selector)

  return effect {
    if prepared == 0 {
      fail Problem { code: 41 }
    }

    return move value
  }
}
```

`prepared` is computed when `risky` is called; the effect block runs later. In an `effect fn`, every
body statement runs later. Effects are language values but need not be heap objects or universally
interpreted recipes: statically known composition lowers directly into the compiler IR.

Externally visible and directly or mutually recursive functions declare complete contracts. Private
non-recursive functions may infer success, failure, and requirement rows from their bodies without
consulting callers or later statements. A declared contract is a checked upper bound: a body may not
fail with an undeclared type or use an undeclared capability. Tooling should infer and insert
contracts mechanically. Bootstrap contracts do not contain retained-provider dependencies because
bootstrap allocator results and other safe owned results are self-contained.

Typed failures are owned, abortive, and non-resumable. `fail value` transfers an owned nominal value
into the failure channel and has success type `never`; Copy values are copied normally and do not
require a meaningless `move`. There is no throwable hierarchy, implicit conversion, or general
exception mechanism. Propagation runs deterministic cleanup for every affine owner exited by the
typed failure. Traps remain separate process-aborting defects that handlers cannot intercept and for
which bootstrap promises no unwinding or cleanup.

Failure payloads must be detached from lexical borrows and service providers. They may own strings,
paths, vectors, or other self-contained allocations because those values carry their own cleanup
authority; they may not retain arena-backed or provider-dependent storage. `OutOfMemory` construction
is allocation-free. Accumulating compiler diagnostics remains ordinary program logic rather than a
hidden failure-channel policy.

A handler may protect one call, an expression, or a block and selects exact nominal members of the
protected failure row. An unguarded member branch removes that member; a guarded branch does not,
because its guard may reject. A universal branch removes all remaining members. If the protected row
is `E`, completely handled members are `H`, and handler branches may fail with `B`, the outward row is
the normalized `(E - H) | B`. The success type is the normalized union of the protected success value
and reachable recovery values. A branch with type `never` contributes no success member.

The matching branch owns its failure payload. Recovering cleans up an unconsumed payload at branch
exit; re-failing transfers it again. Unmatched members propagate without copying. An effect may be
retried only when its capture modes permit another execution; retry is not a mechanism for cloning
consumed affine inputs.

Service capabilities are nominal interfaces. A requirement-row entry is keyed by capability type and
nominal role and records shared or exclusive access. Omitting a role selects `DefaultRole`. Combining
rows retains the strongest mode for duplicate pairs: exclusive dominates shared. A lexical
environment contains at most one current implementation of each pair, while distinct statically
known roles may use different implementations or the same implementation when ordinary borrowing
permits it. Roles are compile-time selectors, never strings or runtime lookup keys.

Each service operation declares how it borrows its implementation. Calling a capability-and-role-
qualified operation uses the lexically current implementation and contributes its access mode to the
requirement row. Service operations may not consume that implementation. Calling an actor function
on a concrete implementation is an ordinary explicit call and does not use the environment. A
shared capability may mutate hidden implementation state only through an explicitly unsafe
interior-mutation contract; bootstrap allocation therefore retains exclusive allocator access until
such a general contract exists.

Only the module defining a nominal implementation type may declare its conformance to a service.
The conformance maps every interface operation to an existing actor function. A mapped function may
have smaller failure and requirement rows or weaker access needs than the interface, never stronger
ones. An implementation-specific dependency must be consumed while constructing owned implementation
state or handled internally; it cannot appear as a hidden ambient requirement at dispatch time.

Provision specializes an effect value. `Capability.provide` captures an existing implementation by
borrow or move and removes one capability-role entry from the requirement row. It does not by itself
create a cleanup boundary: a borrowed provider follows its lexical owner, while a moved provider is
owned by the resulting lazy effect and survives until that effect is consumed or dropped.

`Capability.provideWith` accepts an acquisition effect and creates a fresh implementation for every
execution. Acquisition failures and requirements compose with the target effect. Successfully
acquired owners clean up in reverse acquisition order after success or typed failure. A provider is
not visible during its own construction. There is no implicit memoization, bootstrap `Layer` graph,
global container, dependency solver, named `Scope`, or service registry. Because traps do not unwind,
`provideWith` promises no trap cleanup in the bootstrap runtime.

Effect reuse is derived from captures rather than represented by separate reusable and single-shot
types. Copy captures are snapshotted when the effect is constructed. Shared borrows permit repeated
shared runs. An exclusive borrowed capture requires exclusive execution and its mutations persist
across runs. A moved affine capture is owned by the effect; if execution consumes it, that effect is
take-once. `Effect.retry` accepts only an effect whose captures allow repetition: execution locals
are reconstructed per attempt, while captured mutable state persists. Providers acquired inside the
retried effect are reacquired; captured providers are reused.

Ordinary callable values retain ordered parameter and result contracts plus shared, exclusive, or
consuming invocation mode. Effect values retain success, failure, requirement, and run-access
contracts. Referencing an open `effect fn` captures nothing; supplying its arguments constructs a
closed effect. Supplying trailing arguments to an ordinary multi-argument function instead creates
an automatic unary callable section. Both use compiler-shaped environments, but only the Effect body
is lazy.

Reusable higher-order functions accept ordinary callable contracts. Effect combinators such as
`map`, `flatMap`, `tap`, and `catch` store those callables and derive the resulting Effect run access
from both input Effect and callback: an exclusive callback makes the composition exclusively
reusable, while a consuming callback makes it take-once and therefore ineligible for retry. Such
contracts may quantify over failure and access-qualified requirement rows so effectful callbacks
preserve their rows through finite monomorphization. The rows are not runtime values or general
row-level programming.

The canonical combinators are ordinary visible Silk declarations, not compiler-recognized recipe
names. Their closed compiler core consists of lazy construction, propagating `run`, typed `fail`,
`Effect.result` for reifying a completed typed outcome as `Result<A, E>`, and
`Effect.bindRequirement` for satisfying one capability-role entry while preserving an inferred
remainder. `mapBoth`, `map`, `mapError`, `flatMap`, `tap`, whole-channel `catch`, `retry`, `provide`,
and `provideWith` are derived in `packages/compiler/stdlib/silk/effects.silk`. Failure and
requirement row parameters specialize and erase; neither becomes a runtime record. Effect
parameters use ordinary access bounds—`Effect`, `mut Effect`, and `once Effect`—so reusable APIs
cannot accidentally accept a take-once computation.

Service requirements lower to hidden slots in canonical capability-and-role order. Each slot is a
non-owning opaque implementation pointer plus compiler-shaped conformance witness table. Capability
operations dispatch through statically known offsets; roles have no runtime representation. Pure
functions receive no slots, and running an open effect receives only unresolved slots. Later
optimization may devirtualize statically known witnesses.

Typed failures lower to explicit discriminated success-or-failure results and ordinary branches,
not C++ exceptions or platform unwinding. A caller branches on the result, continues with success,
or runs compiler-planned cleanup and forwards the owned failure. LLVM or Wasm may choose a private
target-appropriate ABI shape without changing this semantic contract.

Unresolved rows may not cross the native executable boundary. A typed host adapter constructs the
approved providers, specializes and runs the user entry effect, exhaustively handles remaining typed
failures, converts the result to the platform exit convention, and drops its owners. The generated
machine entry has empty failure and requirement rows.

All syntax above is semantic notation except where issue 08 has fixed it. Issue 08 owns the complete
concrete spelling for effects, contracts, failures, handlers, provision, access modes, and contract-
row parameters.
