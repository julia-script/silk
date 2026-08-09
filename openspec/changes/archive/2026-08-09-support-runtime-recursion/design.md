## Context

See proposal.md — Why. Instance discovery is already finite and monomorphic, MIR can refer to a
function already present in the instance set, and both code generators emit ordinary calls. The
bootstrap evaluator instead threads a canonical-function active path and rejects any repeated
identity, conflating recursive activation with non-termination.

## Goals / Non-Goals

**Goals:**

- Execute terminating direct and mutual recursion without using the host JavaScript stack as policy.
- Preserve independent locals, borrows, cleanup, and traces for repeated function identities.
- Bound evaluator work deterministically and graduate recursive quicksort across all engines.

**Non-Goals:**

- Prove termination or promise tail-call optimization.
- Impose evaluator limits on emitted native or WebAssembly programs.
- Permit polymorphic recursion or recursive storage layouts.

## Decisions

### D1: Replace recursive host calls with an explicit activation machine

Evaluation uses an explicit stack of activation records. Each record contains a monotonically
assigned frame id, canonical function instance, instruction/region continuation, local values,
borrowed cells, pending call destination, cleanup state, and activation depth. A call pushes a
record after evaluating arguments left-to-right; a return or typed failure unwinds through the
existing cleanup plan and resumes the saved continuation.

Keeping continuations in data avoids JavaScript stack overflow and makes repeated identities
unambiguous. Merely relaxing the active-path guard while retaining host recursion was rejected
because the configured call-depth limit could exceed the host's safe stack and defects would bypass
typed blocked outcomes.

### D2: Count canonical MIR operations and active frames

`Analysis.evaluate` accepts optional `{ maxSteps, maxCallDepth }`, defaulting to 1,000,000 operations
and 1,024 active frames. The evaluator checks the step budget before executing an operation and the
depth budget before pushing a call. Tests use smaller explicit limits for non-terminating fixtures.

This is deterministic execution fuel, not cycle detection. Function identity or argument equality
cannot prove a cycle in the presence of references and mutable state.

### D3: Replace `RecursiveCycle` with one structured `EvaluationLimit` reason

The reason carries `kind: 'Steps' | 'CallDepth'`, configured limit, current count, active function,
stopping span, and ordered active frame identities. The trace prefix remains attached to the blocked
outcome. One reason keeps consumers simple while preserving which resource was exhausted.

### D4: Give trace events activation identity

Entry, call, binding, return, cleanup, and blocked events gain frame id and depth where they are
activation-specific. Canonical function identity remains for source navigation and artifact
stability. This lets the inspector distinguish several active quicksort frames without inventing
source declarations.

### D5: Treat code generators as parity surfaces, not presumed implementations

Acceptance first proves direct and mutual recursive calls in generated native and Wasm artifacts,
including mutable-slice writeback and cleanup. If either backend currently assumes an acyclic call
graph, that assumption is removed within this change; no recursion-specific runtime primitive is
introduced.

### D6: Graduate the existing quicksort unchanged

The recursive source remains Lomuto quicksort. Only its manifest status/blockers change after the
evaluator, native process, and direct Wasm result agree. This prevents an iterative rewrite from
hiding the language wall.

## Risks / Trade-offs

- [The evaluator refactor disturbs cleanup or failure propagation] → Port operations incrementally
  and gate recursive return, typed failure, mutable borrow, and Drop behavior with focused traces.
- [One million default steps makes accidental loops expensive] → Preserve caller overrides and use
  low limits in interactive inspector presets; revise the default only as an explicit tooling policy.
- [Native stack exhaustion remains host-dependent] → Do not execute intentionally divergent programs
  in native/Wasm conformance; emitted runtime quotas are a separate future capability.
- [Trace schema growth affects snapshots and inspector rendering] → Update encoders and accessible
  views in the same change, with fresh-process determinism gates.
