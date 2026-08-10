## Context

Silk currently has three different service-provision shapes hiding behind two general Effect APIs
and one temporary allocator-specific intrinsic:

- `Effect.provide` borrows a provider through `&C` and satisfies a shared requirement;
- `Effect.provideWith` runs an acquisition Effect, owns the resulting provider for one execution,
  and satisfies an exclusive requirement through `&mut C`; and
- `Allocator.provide` borrows an existing allocator through `&mut Allocator` and satisfies the
  exclusive allocator requirement.

The allocator alias exists because an ordinary generic Silk wrapper cannot yet express the whole
exclusive borrowed case. The wrapper must forward an exclusive reference parameter into
`Effect.bindRequirement`, and its provider type can differ from the abstract capability type. For
example, an Effect may require `Allocator` while the value passed to `provideMut` is a
`SystemAllocator` that implements it.

HIR and lowering already preserve a requirement binding whose capability and provider are generic
types. Once a callable instance is concrete, lowering can resolve the implementation witness. The
missing pieces are accepting this deferred obligation during elaboration and diagnosing a missing
witness before backend lowering.

## Goals / Non-Goals

**Goals:**

- expose `Effect.provideMut` as ordinary, navigable Silk source;
- support both data-first and piped calls for any service implementation;
- preserve exclusive borrowing and write mutations back to the caller's provider;
- validate provider conformance for every concrete callable instance;
- remove allocator-specific provision from the intrinsic catalog; and
- keep provision compile-time and zero-lookup at runtime.

**Non-goals:**

- a single access-mode-polymorphic `provide` operation;
- role-polymorphic provision or new syntax for forwarding arbitrary role tokens;
- dynamic runtime service maps or type-erased provider lookup;
- changing `provideWith` acquisition, ownership, or cleanup semantics; and
- effect suspension, fibers, or concurrency runtime work.

## Decisions

### Keep shared, exclusive, and acquired provision explicit

The public Effect API will use three names with distinct ownership contracts:

```silk
Effect.provide(&provider)
Effect.provideMut(&mut provider)
Effect.provideWith(acquireProvider())
```

`provide` borrows shared, `provideMut` borrows exclusively, and `provideWith` acquires and owns a
provider for one run. The explicit `Mut` suffix makes the caller-visible aliasing constraint clear
and avoids inventing an access-mode generic before Silk has a compelling broader use for one.

### Define `provideMut` with separate capability and provider types

The standard-library declaration will distinguish the required capability `C` from the concrete
provider `P`:

```silk
pub effect fn provideMut<A, C, P, !E, ?R>(
  self: once Effect<A ! E ? &mut C | R>,
  provider: &mut P
) -> A ! E ? R {
  let bound = Effect.bindRequirement(move self, provider)
  return run bound
}
```

In a piped call, the section can infer `P` from the explicit provider argument while deferring `C`
until the input Effect is applied. In data-first form, ordinary call inference obtains both types
from their corresponding arguments. This avoids forcing the concrete implementation type to equal
the service capability.

Silk does not yet expose generic conformance-bound syntax. The compiler-owned
`Effect.bindRequirement` core therefore records the `P implements C` obligation when either side
is still generic. This is narrowly scoped to the existing service-binding primitive rather than a
new general-purpose hidden bound system.

### Treat exclusive reference parameters as valid forwarding sources

Exclusive provision currently accepts a mutable local binding but rejects a function parameter,
even when that parameter has type `&mut P`. Elaboration will derive exclusivity from the provider
expression's reference type and access path. It will allow forwarding an exclusive parameter while
continuing to reject shared references, immutable values, and paths that do not carry exclusive
access.

This does not create a new borrow: the wrapper forwards the caller's scoped borrow into the bound
Effect. The resulting Effect cannot outlive that borrow, and mutations remain observable through
the original provider after execution.

### Defer generic conformance, then validate every concrete instance

When `Effect.bindRequirement` sees concrete nominal types, elaboration will continue resolving the
implementation witness immediately. When capability or provider types are generic, it will retain
both semantic types in the HIR without requiring an early witness.

Instance realization will substitute the concrete types and verify that the provider equals or
implements the required capability. A missing witness produces the normal invalid-provision
diagnostic before MIR or backend lowering. Lowering then consumes the already-valid concrete pair
and resolves the same witness for dispatch metadata.

This split keeps generic library code reusable while ensuring that invalid calls fail as source
errors rather than internal lowering failures.

### Specialize the forwarding shape, not the `provideMut` name

An exclusive provider borrow cannot be reified as an ordinary value around a compiler recipe such
as `Allocator.allocate`: the recipe deliberately has no runtime Effect identity. Instance
discovery and lowering will therefore recognize the exact source structure “bind one exclusive
requirement, then immediately run the bound Effect” and specialize that wrapper into the caller.

The specialization is structural. It applies to any source declaration with the same HIR shape,
retains the caller's original borrow expression, and does not inspect the declaration name, actor,
or standard-library module. Calls whose input is already a reified Effect continue through the
ordinary effect-call path. This keeps `provideMut` library-defined while allowing compiler recipes
to flow through it without allocating or dynamically dispatching an intermediate Effect.

### Remove the allocator alias instead of preserving compatibility

`Allocator.provide` will be removed from the intrinsic catalog, tests, examples, labs, and pressure
programs. All borrowed allocator provision will migrate to `Effect.provideMut`. The project is
pre-release, so retaining the alias would only preserve an accidental privileged API and create
technical debt.

`Effect.bindRequirement` remains the compiler-owned core primitive. User-facing service APIs are
ordinary Silk declarations built over it.

## Risks / Trade-offs

- **Deferred obligations could escape validation.** Instance realization must walk every reachable
  requirement-binding expression after substitution. Tests will cover direct, generic, piped, and
  invalid-provider calls so backend lowering never becomes the first validator.
- **The compiler-owned core still knows about conformance.** That knowledge is inherent to removing
  a statically typed service requirement. Keeping it at `bindRequirement` avoids recognizing
  `provideMut` by name and lets user code define equivalent wrappers.
- **Structural specialization adds a narrowly defined lowering rule.** Requiring an exact
  bind-then-run body prevents unrelated functions from being rewritten, while retaining the
  zero-cost path for both data-first and piped calls over compiler recipes.
- **Separate `C` and `P` add one generic parameter to the source definition.** Callers infer both,
  so the additional precision is invisible at use sites and prevents false equality constraints.
- **Role forwarding remains incomplete.** This change handles the current default service role. A
  future role-polymorphic API should be designed with explicit role parameters rather than folded
  implicitly into this proposal.

## Migration Plan

1. Add compiler acceptance tests for exclusive parameter forwarding, generic implementation
   conformance, data-first and piped calls, mutation writeback, and invalid providers.
2. Extend `Effect.bindRequirement` elaboration and concrete-instance validation.
3. Add `Effect.provideMut` to the shipped Silk standard library and regenerate its embedded source.
4. Remove `Allocator.provide` intrinsic registration and migrate every repository call site.
5. Run compiler-focused tests, the full repository checks, and release-candidate validation.

No rollback compatibility alias is planned. Reverting the change would restore the intrinsic
catalog entry and previous call sites together.

## Open Questions

- What syntax should eventually express generic conformance bounds in ordinary Silk APIs?
- Should a later role system allow provision to infer a role from the requirement row, or require
  the role to be explicit at the call site?
