## Context

The minimized source is independent of TLS and uses only an ordinary user service, a generic
provider, `Option<P>`, a local `effect fn` callback, and `Effect.useReleaseNonParking`. Semantic
analysis succeeds. Final MIR nevertheless contains a `RunEffect` in the bracket runner targeting
the specialized local callback runner while the module's function set does not contain that target.
The verifier therefore reports `runner=false` while `effect=true`, `stored-contract=true`,
`static-runner=true`, and `propagation=true`.

Three reductions locate the defect after semantic call-shape construction and inside final runner
lowering:

- storing `P` directly instead of in `Option<P>` makes the same generic provision compile;
- retaining the `Option<P>` pattern but removing the provided service call compiles; and
- removing callback captures, residual requirements, suspension, release-time provision, and
  failure recovery does not remove the failure.

The executable and provisional closures already retain both the callback execution and its runner,
and layout records the callback's exact environment. Final lowering then rejects the forwarded
provider solely because its borrow root is an active pattern binding, even though ordinary borrow
lowering and ownership already resolve that root to the selected place. The correction seam is the
structural forwarded-provider eligibility check: it must admit the same pattern-rooted borrow that
the ownership-aware lowerer already supports. Final MIR verification is correctly detecting the
runner omitted after that lowering failure.

## Goals / Non-Goals

**Goals:**

- Preserve one exact reachable callback-runner edge from an ordinary bracket construction through
  generic specialization, pattern selection, and service provision.
- Reuse the compiler's canonical callable identity, concrete substitution, conformance witness,
  provider binding, and Effect runner identity rather than reconstructing any of them.
- Keep discovery finite and deterministic and make final MIR pass the existing strict verifier.
- Keep a small structured compiler regression whose failure reports the missing runner directly.

**Non-Goals:**

- No TLS, native-socket, HTTP, or standard-library-specific compiler recognition.
- No new intrinsic, source syntax, type-system rule, Effect combinator contract, runtime Effect
  representation, or backend-specific lowering path.
- No weakening of `InvalidEffectOperation`, fabrication of a placeholder body, forced synchronous
  classification, or catch-all retention of every specialized runner.
- No implementation or completion of `add-owned-tls-connections` in this compiler change.

## Decisions

### Extend structural forwarded-provider lowering, not verification

Implementation will preserve the already selected callback identity and provider proof while
allowing the forwarded-provider path to lower a `ValueBorrow` rooted at the active match binding.
That path already lowers the provider expression with the ordinary ownership-aware expression
lowerer before constructing the exact provided requirement; no synthetic pattern field or new
runtime representation is needed. The diagnostic and provisional evidence prove that discovery,
layout, result/call-shape construction, and verification are already correct.

Alternative: accept the missing runner in `MirVerification`. Rejected because a backend cannot call
an absent function, and the verifier's current evidence accurately separates the missing target
from valid stored-contract, static-runner, and propagation facts.

Alternative: retain every callback/provider specialization. Rejected because it would hide missing
dependency edges, inflate artifacts, and weaken the repository's finite-reachability discipline.

### Reuse the canonical provider selection

The correction uses the existing recorded callable environment, specialization lookup, conformance
proof, and ownership place. It preserves the callback's exact type arguments, provider
witness/access, Effect outcome, and cleanup dependencies. Pattern syntax and the names `Option`,
`useReleaseNonParking`, `Transport`, or TLS are not semantic selection keys; the builtin bracket
participates only through its existing general callback-ordinal contract.

Alternative: teach lowering to recover a function from the target text on verifier failure.
Rejected because string recovery is non-canonical, too late to preserve provider and cleanup facts,
and contrary to minimal compiler privilege.

### Prove the correction before any backend work

The permanent regression performs one analysis snapshot and asks the existing MIR verifier to
accept the lowered module. Controls establish that generic provision alone and nominal-union
selection alone already work, so the new case has a distinct failure oracle. Existing native and
Wasm acceptance later exercise the same shared MIR path; this defect does not justify another
backend compilation pass.

## Risks / Trade-offs

- [Admitting a pattern-rooted provider could bypass ownership] → Continue lowering the original
  `ValueBorrow` through the ordinary ownership-aware expression path; do not reconstruct a root or
  local from pattern spelling.
- [The repaired callback could be retained twice] → Reuse the current canonical runner identity
  and assert deterministic single retention in the structured regression.
- [The fix could accidentally classify a synchronous callback as suspendable] → Preserve existing
  classification facts; the regression needs only runner presence and must not add suspension,
  coroutine, or backend-runtime claims.

## Migration Plan

There is no public migration. Land the compiler reachability correction with the permanent
regression, then resume `add-owned-tls-connections` without changing its approved API or design. A
rollback removes the correction and restores the known verifier failure; it does not require a
source or data migration.

Until that apply completes, `add-owned-tls-connections` cannot close tasks 3.2 through 3.6 or the
executable/migration portions of 4.1 through 4.4: the shipped TLS actor analyzes, but any portable
lowering that reaches its generic owned bracket fails before runtime evidence can execute. Tasks
1.x, 2.x, and the analysis-only part of 3.1 do not depend on this correction.
