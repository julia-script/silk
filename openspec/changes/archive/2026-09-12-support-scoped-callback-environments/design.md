## Context

See `proposal.md` for motivation and `investigation.md` for the experiment log. The selected base
is local JUL-188 checkpoint `d1d707e6`, not its incomplete published branch. Existing rules in
`apps/docs/content/reference/lifetimes.md` distinguish invocation lifetimes from callable and
Effect retained environments. They require ordinary variance and reject strengthened offered
validity. They do not define a source-level lifetime intersection.

`DeclarationFacts.executableLifetimes` synthesizes a fresh environment when retained inputs do
not have a known common lifetime. For the reproduction it produces `'env1` with
`'env: 'env1` and `'scope: 'env1`. `ExpressionAnalysis.analyzeAnonymousCallable` quantifies the
borrowed argument but leaves the returned environment distinct. `inferQuantifiedExecutable`
opens the resource borrow rigidly. The old bracket requires `'env1: 'scope`; neither the
recorded bounds nor ordinary variance prove it. `rowInferenceFailure` then reports an open row,
which hides this earlier lifetime mismatch. A concrete row fails too.

The current bounds make `'env1` a common lower bound. They do **not** prove it is the greatest
lower bound, or license reversing either edge. This proposal introduces that missing semantic
expression explicitly; it is not a reinterpretation of all existing fresh lifetime variables.

## Goals / Non-Goals

**Goals:** a bounded, target-neutral contract for an Effect returned under a fresh resource loan
and a fixed captured environment; precise generic channels; unchanged resource and cancellation
obligations. The extension must work for ordinary user-defined services and functions.

**Non-Goals:** a general higher-rank solver, lifetime unions, lifetime-selected dispatch, relaxed
exclusive payload invariance, an erased callback ABI, a new runtime bracket, or changes to the
TLS implementation beyond applying the eventual static contract to its preserved caller.

## Decisions

### 1. Express the returned environment as a finite intersection

Proposed source spelling (not accepted by the current compiler):

```silk
for<'scope> once fn<'env>(
  &'scope mut Resource
) -> once Effect<'scope & 'env; A ! E ? R>
```

Initially admit intersection expressions in Effect environment positions, including explicit
`effect<'a & 'b> fn` environments. The atom syntax remains a named lifetime. The intersection
is associative, commutative, and idempotent; `'static` is its identity. Flatten, deduplicate, and
sort canonical constituents by semantic identity. Do not normalize using ambient assumptions:
the same declared type must not acquire context-dependent identity. No extra universal binder
is introduced by an intersection.

Let `M = meet(a, b)`. The proof rules are:

- `a: M` and `b: M`.
- If `a: x` and `b: x`, then `M: x`.
- If `x: a` or `x: b`, then `x: M`; retain ordinary transitive/declared proofs as well.
- `M: a` is not generally provable. Exclusive destination payload invariance remains unchanged.

A run remains legal only in a region for which **all** retained dependencies are valid. Having
an intersection type does not itself create a loan, extend a source, move an owner, or prove a
resource is alive. Local region/loan checking must still establish a nonescaping usable run
region; no universal runtime meaning is assigned to an empty common region.

Alternative: existential returned Effect environment or a callback-result family carried only by
the bracket. That could express the desired restriction but needs another binder/representation
contract and introduction/elimination rules. Prefer a finite expression over already known
lifetimes, so the relationship is explicit and ordinary callable composition can use it.

### 2. Derive anonymous environments from real dependencies

For an anonymous Effect-producing callable, form the returned environment from the callable's
retained capture validity and the validity of newly supplied retained arguments. A capture-free
callable has static capture validity, so the existing single-resource case reduces to `'scope`.
A short captured config or callback contributes its real shorter validity.

Carry generic `T: environment` obligations independently. Do not treat an unknown generic
payload as detached or invent a lifetime for it. The well-formedness of `&'scope mut Resource`
still establishes the resource-content obligations needed for that use, while captures must
already satisfy the callable's own validity contract. Preserve ownership transfer of captured
`once` callables and reject reuse; lifetime intersections cannot authorize copying them.

Do not globally equate existing synthesized lower-bound binders with an intersection. Migrate
only derivations whose complete dependency set and type-validity obligations are known; retain
explicit constrained environments for other generic function shapes.

Alternative: force the anonymous result environment to equal the input loan. Rejected because
independent captures can be shorter. Merely deleting `SEM0089` or weakening `inferLifetime`
would leave this unsound promise in place.

### 3. Keep higher-ranked checks rigid and row inference separate

Substitution, free-lifetime traversal, occurrence/escape checks, compatibility, and inference
must recurse through intersections. Opening a callable replaces only its invocation binder
with a rigid placeholder; free captured lifetimes remain free and distinct. Check the finite
intersection expression under that same universe. An inferred success type, failure member,
requirement capability, or representation argument that contains the placeholder is still
rejected on leaving the universe.

Requirement-row specialization keeps its existing finite row algebra and exact forwarding
rules. An independent row-exclusion constraint remains required and checked at the caller.
Do not make an ambient provider available because a callback acquired a shorter environment.
Do not use a later concrete specialization to rescue an invalid generic lifetime proof.

If inference fails at an environment obligation, report that failed obligation rather than
misclassifying it as non-finite row specialization merely because the offered row is symbolic.
This diagnostic improvement must preserve actual non-finite/ambiguous row rejections.

### 4. Update the bracket contract, preserve its ownership mechanism

Change both use and release result environments in the ordinary-source helper and sealed
intrinsic to `meet(scope, env)`. `Resource`, `A`, `E`, `R`, and `S` remain outside the invocation
binder; `Release` keeps its exact representation and `Intrinsic.NonParking` constraint.
The outer computation must retain the resource and both callback environments for its whole
execution. It must invoke use, run and dispose its returned Effect, and end the use loan before
creating the fresh release loan. The intersection returned by one invocation cannot be stored
as a result of the bracket or used after release.

Keep the existing resource-finalizer MIR/runtime mechanism. Verify cancellation while the use
Effect is suspended, typed failure, success, nested release order, and preservation of the
original outcome. Do not implement manual cleanup or bypass NonParking. If existing lowering
cannot satisfy these obligations, stop and report the concrete failure before extending the
runtime scope of this proposal.

### 5. Carry the semantic expression through existing compiler boundaries

Implementation touchpoints, found during the investigation:

- `Lifetime.ts`, `DeclarationLifetime.ts`, parser/type syntax and formatting: canonical expression,
  allowed spelling, traversal, bounds, and stable display.
- `DeclarationFacts.ts`, `ExpressionAnalysis.ts`, `BodyLifetime.ts`: dependency-derived anonymous
  environments and generic type-validity obligations.
- `internal/TypeInference.ts`, `TypeCompatibility.ts`, `Type.ts`: substitution, rigid opening,
  variance, inference failure reporting, and placeholder escape.
- `ModuleSurface.ts` and other lifetime encoders/readers: lossless semantic serialization;
  runtime lifetime erasure must remain complete.
- `Intrinsic.ts`, `stdlib/silk/effect.silk`, generated stdlib/reference content and existing
  bracket tests: one coherent public and sealed contract, with no obsolete alternate helper.

Before editing a touchpoint, inspect its actual exhaustive lifetime consumers; these names are a
bounded implementation map, not a claim that every affected switch has already been enumerated.

## Risks / Trade-offs

- More semantic lifetime structure → use finite canonical constituent lists and existing scoped
  proof contexts; no implementation/provider search or new performance project.
- A generated lower bound mistaken for a meet → derive intersections only from complete retention
  dependencies; never reverse arbitrary outlives edges.
- Capture validity confused with borrowed owner storage → preserve concrete loan provenance,
  captured-owner transfer, cleanup, and outcome escape checks independently of region algebra.
- Placeholder hidden in a compound region → recurse in every free-variable and escape check;
  retain negative higher-ranked/owned-result tests.
- An apparently accepted program fails residual ownership or MIR → require the full tiny
  reproduction to analyze and lower before touching the preserved TLS caller.
- Generic provider `P` obligations may still fail → explicitly verify the preserved correction
  set only after the tiny acceptance gate; report additional failures rather than widening scope.

## Migration Plan

Implement only after explicit approval. Replace the old bracket signature and affected callers
atomically, regenerate stdlib and docs, and retain the existing helper name with its new contract.
There is no compatibility alias or fallback under the repository's green-field policy.

Deliver a separate focused commit range to the paused implementer. Do not rebase or mutate the
original worktree. Broad validation belongs in CI; no full local compiler/TLS sweep is planned.
