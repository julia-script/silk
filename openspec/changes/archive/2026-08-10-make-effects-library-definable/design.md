## Context

See `proposal.md` for motivation. Silk already models Effect success, owned typed failures,
access-qualified requirements, capture-derived run access, hidden construction-site environments,
and finite monomorphization. Its intrinsic catalog even presents pseudo-signatures such as
`Effect<A ! E | Rest>`, but row variables and subtraction are available only to Effect-specific
elaboration. Lowering then executes several HIR recipe variants directly instead of compiling
ordinary source combinators.

The concurrency direction separately requires that closed non-suspending programs pay no scheduler
or fiber cost, while leaving a future path for an execution to park its current fiber. This design
therefore cannot expose the current synchronous runner ABI as the permanent public Effect model.

## Goals / Non-Goals

**Goals:**

- Make the standard Effect API an ordinary, inspectable Silk library over a small compiler core.
- Generalize the type system just enough to express finite A/E/R channel transformations honestly.
- Preserve hidden closure identity, ownership, cleanup, and monomorphic pay-for-use lowering.
- Keep current execution synchronous while making source combinators transparent to future
  suspension.

**Non-Goals:**

- Implementing fibers, a scheduler, async I/O, runtime suspension, interruption, or parallelism.
- Exposing dependency rows, provider roles, runner frames, or a context container as runtime data.
- Recreating Effect's JavaScript instruction interpreter or universal heap-allocating every Effect.
- Preserving compiler-private combinator HIR, MIR, or intrinsic APIs for compatibility.
- Proving zero-cost abstraction; the dependent optimization spike measures that separately.

## Decisions

### Treat Effect as a compiler-shaped closure with three channel contracts

`Effect<A ! E ? R>` remains a source-visible structural contract over one hidden nominal closure
identity. `A` and `E` are outputs; `R` is an input requirement row. Concrete closure fields, provider
slots, and runner calling convention remain compiler-owned. This is preferred over a source
`struct Effect` because its existential environment and future continuation representation must not
become a public ABI.

Conceptually only, execution has this direction:

```text
requirements R -> hidden runner/environment -> Result<A, E>
```

### Add kinded row parameters with finite monomorphic specialization

Generic binders distinguish ordinary values from failure and requirement rows, using channel-marked
binders such as `<A, !E, ?R>`. Row expressions support union and selected-entry-plus-remainder
unification without evaluating a general row-programming language. The initial `catch` transforms
the complete `E` channel, while `provide` uses selected requirement binding to return the inferred
remainder. Selective failure recovery can be added later as an ordinary library operation over the
same finite row algebra.

Rows enter concrete instance keys in canonical normalized order and erase before runtime. This
extends existing finite specialization rather than introducing dictionaries, structural runtime
reflection, or unrestricted polymorphic recursion.

### Keep the compiler Effect core closed and channel-oriented

The compiler owns only:

- construction of a lazy Effect closure;
- propagating execution through `run`;
- owned typed failure through `fail`;
- effectful reification of a completed typed outcome as ordinary `Result<A, E>`;
- introduction and contravariant adaptation/binding of typed capability-role requirements.

Outcome reification is effectful: it preserves the original requirements and may wait through a
future suspension before producing Result data. It does not catch traps or future interruption.
Requirement adaptation is compiler-shaped and typed; there is no public `Requirements<R>` record.

This core is preferred over privileged `mapBoth` because ordinary Result matching derives pure and
effectful transformations of both output branches. It is also preferred over exposing a raw
synchronous callback because that would freeze out suspension.

### Build channel operations and familiar combinators in Silk

The canonical library first defines success mapping, failure mapping, both-branch mapping, and
requirement adaptation. Effectful branch transformations then derive `flatMap`, `tap`, `catch`, and
`retry`; typed scoped requirement binding derives `provide`, while effectful acquisition plus the
same binding derives `provideWith`.

These declarations retain ordinary callable modes and ownership. A composed Effect's access is the
strongest access required by its input Effect and stored callbacks. Row composition follows the
generic contracts and ordinary body calls, not declaration-name recognition.

### Preserve suspension behind run and outcome reification

Library code observes only completed typed outcomes. If suspension is implemented later, compiler
lowering may conceptually produce either completion or a private continuation, but `run` and outcome
reification suspend the current execution transparently. An Effect is not a fiber: sequential
composition continues on the current execution, and only a future fork creates an independent
fiber.

The synchronous implementation remains direct and links no scheduler. The compiler may later infer
non-suspension and select a direct Result-returning runner or compile suspendable bodies to state
machines without changing the source Effect API.

### Migrate differentially, then delete privileged combinators

Introduce the type/core facilities and source library alongside the existing intrinsics long enough
to compare semantic facts, ownership, evaluator, native, and direct-Wasm behavior. Switch name
resolution to the canonical source declarations only after parity. Then delete Effect-specific
combinator elaboration, recipe bookkeeping, lowering operations, generated signatures, and tests
that assert private representation.

This controlled replacement is preferred over rewriting every feature at once. The current
`compose-effect-pipelines` work may remain a small compatibility bridge for accepted programs, but
must not become the permanent architecture or expand the privileged recipe algebra further.

## Risks / Trade-offs

- **[Risk]** Row inference becomes ambiguous or permits infinite specialization. **Mitigation:** use
  kinded finite rows, canonical normalization, selected-entry-plus-remainder constraints, and the
  existing record-before-follow monomorphic worklist.
- **[Risk]** Passing hidden Effect values through ordinary functions erases construction identity.
  **Mitigation:** retain hidden nominal environment identity in semantic facts, instance keys,
  ownership, and layout while exposing only the structural contract to source.
- **[Risk]** Source combinators change callback construction, failure, or cleanup order.
  **Mitigation:** differentially compare order-sensitive Copy and affine programs before deleting
  intrinsics.
- **[Risk]** Outcome reification accidentally catches traps or future interruption. **Mitigation:**
  reify only the typed Result channel and keep abnormal/runtime exits outside `E`.
- **[Risk]** A future suspension ABI leaks into every synchronous program. **Mitigation:** keep it
  private, preserve the direct synchronous path, and use the dependent cost spike before selecting
  an optimization strategy.

## Migration Plan

1. Add kinded row facts, normalization, constraints, deterministic encodings, and tooling.
2. Make Effect values valid ordinary higher-order parameters/results and add outcome reification and
   typed requirement adaptation to the compiler core.
3. Add the canonical Effect Silk module and prove `mapBoth`, `map`, and `mapError` first.
4. Port effectful success/failure transformations, retry, and both provision forms with ownership
   and cleanup parity.
5. Switch resolution/tooling to the source declarations, remove privileged combinator semantics,
   and run release/package gates.

Rollback before step 5 keeps the old intrinsics selected. After step 5, rollback is the complete
change because retaining two semantic sources would violate the no-privilege requirement.
