## Context

Entry-local WAT from the cost spike shows three calls for the simple map entry: two trivial
constructors and one statically selected runner. The constructor functions merely capture their
parameters in `MakeEffect` and return. `RunEffectValue` then unpacks that same environment to call a
runner already named in MIR. These are representation round trips; the Effect library's control
semantics live in the runner body and are a separate optimization problem.

Silk has no suspension operation today. The normalization still records its synchronous premise so
future suspension support can turn it into a real guard instead of silently widening eligibility.

## Goals / Non-Goals

**Goals:**

- Remove direct calls whose complete body is one Effect construction and return.
- Remove a local take-once Effect environment materialization immediately consumed by its selected
  runner.
- Make acceptance and first rejection reasons deterministic and inspectable.
- Preserve failure propagation, provider arguments, traps, semantic runtime observations, and
  Copy/shared ownership. Representation-level call/binding trace events are expected to disappear
  with the folded calls.
- Feed the same normalized MIR to the evaluator and both backends.

**Non-Goals:**

- Inlining runner or callback CFGs, scalarizing `Result`, or matching Effect API names.
- Optimizing escaping/reused/mutable Effects, dynamic callables, recursive constructors, or affine
  captures in this slice.
- Defining suspension, fibers, a scheduler, or `MaySuspend` inference.
- Repairing the separate captured-affine-owner transfer defect found by the cost spike.

## Decisions

### 1. Recognize a constructor by body shape

A direct `Call` or direct `ApplyCallable` is foldable only when its concrete callee has one
`OperationRegion`, contains exactly one `MakeEffect`, and returns that operation's destination.
Parameter and capture substitution must be total, and the function must have no cleanup, branches,
loops, traps, loans, or other operations. The replacement is a caller-local `MakeEffect` with the
same runner identity, generic arguments, capture modes, result type, and call-site provenance.

This rule is deliberately independent of declaration/module names and therefore applies equally to
copied user combinators.

### 2. Introduce one direct static-run MIR operation

When one caller-local `MakeEffect` is consumed exactly once by a following `RunEffectValue`, replace
both with `RunStaticEffect`. The operation carries the runner identity, substituted capture locals,
provider arguments, outcome local, failure mapping, releases, and original source provenance. It has
the same runner-call and semantic observation contract as `RunEffectValue`; it merely avoids
constructing and immediately unpacking an environment.

The first slice accepts only Copy/shared captures. Take/exclusive captures, any additional use,
cross-region use, or unknown ordering retain ordinary MIR so ownership remains explicit.

### 3. Publish canonical verdicts

The normalized module records verdicts in function/region/operation order. Accepted verdicts name
`FoldedConstructor` or `DirectStaticRun` and the proven guards. Rejections name only the first
canonical reason, such as `ComplexConstructor`, `EffectEscapes`, `EffectReused`, `AffineCapture`, or
`SuspensionUnknown`. Verdicts carry originating provenance and referenced function/region/local
identities so the verifier can reject malformed observations.

### 4. Normalize once at the shared boundary

`Pipeline` invokes the actor immediately after lowering, before snapshots, evaluation, or backend
emission. Running it again is idempotent: folded calls are gone and direct static runs are not
candidate inputs. Rejected candidates are structurally identical to lowered MIR.

### 5. Separate the next optimizer

After this slice, a direct-Wasm entry may still call the selected runner. Removing that call requires
cloning an arbitrary region graph, remapping success/failure/return/trap exits, and transferring
cleanup ownership. That work must be proposed separately and gated by affine fixtures; it is not an
incremental extension hidden inside constructor folding.

## Risks / Trade-offs

- **Semantic observation drift** → Give `RunStaticEffect` the existing `RunEffectValue` runner-call,
  failure, allocation, and cleanup behavior; only representation-level constructor call/binding
  events disappear.
- **Capture ownership drift** → Accept only Copy/shared captures initially and keep affine cases as
  explicit rejected controls.
- **Name privilege** → Recognize only MIR operation/body shape and test a copied user constructor.
- **Future suspension** → Require a synchronous verdict produced from the current no-suspension MIR
  vocabulary; reject `Unknown` in synthetic verifier coverage.
- **Small structural win** → Measure entry calls separately and do not claim runner-body elimination.

## Migration Plan

1. Add verdict and direct-run representation plus verifier/evaluator/backend support.
2. Implement normalization behind an explicit option and prove parity/idempotence.
3. Enable it in both analysis realization and driver preparation.
4. Record remaining runner calls as the input to a distinct CFG-inlining proposal.

Rollback removes the shared normalization invocation; source-defined Effects and ordinary MIR remain
valid.
