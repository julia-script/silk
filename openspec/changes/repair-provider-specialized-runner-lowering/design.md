## Context

The existing single-caller owned TLS/Wasm composition verifies and executes, while adding only a
second `withClient` caller using a different callback makes target-neutral MIR verification fail.
The reduced diagnostic retains the normal success exchange plus the truncation callback and takes
approximately 116 seconds to analyze and lower. It produces four `InvalidEffectOperation`
violations, all for one absent `OwnedConnection.readSome` provided runner; the references occur in
the provided truncation callback, `Effect.map`, and the original success callback.

Instrumentation established the boundary precisely: `ensureProvidedRunner` finds the open base and
creates the provided specification, but `lowerEffectRunner` receives that specification and returns
no function because its body sequence becomes unavailable. The generated-runner loop silently
discards that failed result, leaving otherwise valid callers to reference an absent target. The
earlier generic-bracket correction remains green and does not cover this second-specialization
failure.

The minimized regression isolated two adjacent losses. A callable section selected in a second
proof context had no exact semantic layout entry even though it shared the first context's canonical
runtime environment. After guarded recontextualization admitted that section, direct Effect-run
lowering exposed the next loss: exact call lookup rejected the retained call solely because local
lifetimes differed, then emitted authored arguments without the discovered hidden callable
identity. A focused existing bracket case also showed why failure validation belongs after the
generated-runner fixed point and why a captured Effect's layout-resolved canonical identity must
survive into parameter lowering.

## Goals / Non-Goals

**Goals:**

- Identify the first unavailable statement or expression while lowering a generated runner without
  reconstructing the cause from a later verifier violation.
- Lower the second concrete caller specialization through the same canonical provider, ownership,
  and Effect paths as the already working first specialization.
- Keep generated-runner discovery finite, deterministic, and demand-driven.
- Replace the TLS-derived diagnostic with the smallest structural regression that preserves the
  second-specialization failure.

**Non-Goals:**

- No TLS, `OwnedConnection`, `ByteDuplex`, `Effect.map`, or source-module name recognition.
- No new intrinsic, syntax, service rule, standard-library API, runtime representation, backend
  path, or ABI.
- No global retention of generated runners, retrying every failed runner without evidence, forced
  synchronous/suspendable classification, placeholder runner bodies, or verifier relaxation.
- No implementation or acceptance-task completion for `add-owned-tls-connections` in this change.

## Decisions

### Make generated-runner lowering failure explicit at its origin

The generated block-runner lowerer will return a discriminated internal outcome rather than using
`undefined` for both ordinary optional discovery and a failed reachable body. The unavailable arm
will carry the generated runner identity, its open base, its owner instance, and the first failure
recorded while lowering the body. The body recorder will prefer an exact expression boundary when
the rejecting lowerer has one and otherwise fall back to the containing statement span and kind.

The top-level generated-runner worklist will treat an unavailable reachable runner as a compiler
invariant failure and stop before backend emission. It will not publish a partial module that only
later reveals the absent callee. Invalid source remains fenced by existing semantic diagnostics;
this internal result does not create a new user-recoverable source error.

Alternative: rely on `InvalidEffectOperation` after module construction. Rejected because it names
the callers of the absent runner, not the lowering boundary that discarded the runner, and permits
expensive backend-oriented preparation before exposing the compiler defect.

Alternative: fabricate a trap runner. Rejected because it would convert valid source into different
runtime behavior and make the strict verifier accept an implementation omission.

### Repair the first recorded lowering boundary, not the worklist broadly

Implementation will use the explicit failure to locate the first statement/expression that rejects
the second provided specialization. The repair will preserve that operation's existing canonical
effect identity, concrete substitution, stored realization, provider witness/access, and ownership
plan. `ensureProvidedRunner` and the global reachability policy will remain unchanged unless the new
evidence contradicts the observed successful specification creation.

Fail-closed validation also exposes capture failures before a body expression exists. When layout
resolves an Effect capture's representation-site alias to one canonical environment, that resolved
identity will travel as internal field metadata into runner parameter lowering. The lowerer uses the
resolved identity first and retains the authored alias only as its existing fallback. This does not
change field count, layout, ABI, or reachability; it prevents a later phase from discarding the
deterministic resolution that layout already proved.

Alternative: retry failed generated runners after the worklist grows. Rejected at proposal time
because instrumentation observed the required base before failure. A retry would mask an
order-dependent lowerer without explaining why the same complete specification initially failed.

Alternative: retain every provided runner. Rejected because it hides dependency mistakes and adds
unreachable code and compiler work.

Alternative: recover a missing captured Effect by matching only its runtime type. Rejected because
multiple Effects can share one contract and representation while naming different runners. The
canonical identity established by layout is the required provenance.

### Use one target-neutral structural regression

The permanent regression will live in an existing compiler test file, build one analysis snapshot,
and assert empty semantic diagnostics, a present exact provided runner, and clean strict MIR
verification. The current TLS-derived source is an investigation oracle only. Before landing, it
must be reduced to ordinary user-defined services/generic actors or folded into an existing snapshot
whose incremental cost is justified. No LLVM, Wasm, native execution, fresh process, or extra
backend profile is needed: all observed failures occur before a backend.

An intentional internal negative will build the same unavailable outcome emitted by the block
runner producer and feed it through the pure post-pruning selection seam with retained runner keys.
It will prove that a retained failure throws with runner/base/owner/provenance intact while an
unreachable failure is ignored. It must not require a second source analysis.

Alternative: keep the 116-second TLS reduction as a permanent test. Rejected because its diagnostic
value does not justify repeating the full TLS realization in the default compiler suite.

## Risks / Trade-offs

- [Failure plumbing expands across many expression lowerers] → Record the first cause in the
  generated-runner `FunctionLowering` context and add precise producers only along the reproduced
  failing path; retain a statement-level fallback instead of converting every optional lowering API.
- [A local fix accidentally depends on discovery order] → Run the minimized two-specialization
  source with caller order reversed and require equivalent runner identities and clean MIR.
- [The failure outcome becomes a user semantic diagnostic] → Keep it compiler-internal and fail
  artifact preparation before backend emission; invalid source continues through existing semantic
  diagnostics.
- [The repair retains duplicate or unreachable functions] → Assert one canonical emitted function
  per referenced runner identity and retain the existing reachability pruning.

## Migration Plan

There is no source or runtime migration. Land the internal lowering result, the exact
second-specialization repair, and its structural regression together. Then resume the existing
owned-TLS change and rerun its already-defined target-neutral, Wasm, and native acceptance without
changing the approved TLS API.

Rollback removes this compiler correction and restores the known MIR failure; it does not require
data, API, or generated-artifact migration.
