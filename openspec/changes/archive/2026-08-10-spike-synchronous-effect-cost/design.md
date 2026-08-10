## Context

See `proposal.md` for the motivation. This spike follows `make-effects-library-definable`: it needs the real source-defined combinators, erased row metadata, and abstract runner representation from that change rather than a synthetic TypeScript approximation.

Silk already lowers `|>` into ordinary callable application before MIR, so pipe syntax and Effect representation are separate variables. Native compilation passes through LLVM, while the direct-Wasm backend does not; evidence from one backend cannot stand in for the other. Silk does not yet have a suspension operation or continuation representation. The spike therefore establishes the synchronous baseline and a regression guard against accidentally introducing scheduler, fiber, or suspension machinery before those semantics exist.

## Goals / Non-Goals

**Goals:**

- Make the cost of source-defined, synchronously completing Effect composition observable at every relevant compiler boundary.
- Compare semantically matched programs rather than isolated compiler-generated fragments.
- Distinguish frontend costs, runner-representation costs, and backend-specific optimization gaps.
- Establish explicit evidence thresholds for deciding whether further compiler work is warranted.
- Record the exact limitation that future suspension cannot be measured yet and define the structural baseline that a future suspension design must preserve for proven non-suspending code.

**Non-Goals:**

- Implementing a MIR optimizer, changing the runner ABI, or adding suspend/fork/scheduling behavior.
- Selecting benchmark numbers as a stable performance promise.
- Proving all Effect programs are zero-cost; dynamic callbacks, escaping Effects, and genuinely suspending work may retain representation costs.
- Comparing Silk with Rust, Zig, Effect TypeScript, or another language in this spike.

## Decisions

### 1. Use a paired public Silk corpus

Each measured case has an imperative or explicit-`Result` baseline and a library-defined Effect counterpart with the same observable inputs, outputs, failure behavior, requirement behavior, evaluation order, and ownership profile. Cases cover:

- pure `|>` composition without Effects, to confirm pipe erasure;
- `map` chains;
- `mapBoth` on success and failure paths;
- `flatMap` sequencing;
- requirement provision and adaptation;
- direct composition, composition through a named helper, and a stored Effect value;
- representative `Copy` and affine values; and
- a synchronous-runtime guard proving that scheduler, fiber, continuation, and suspension-dispatch machinery is absent from current artifacts.

The pressure corpus uses familiar, small computations rather than artificial identity-only expressions, while keeping every pair small enough to inspect. This makes findings attributable and also exercises the language people will actually write.

**Alternative considered:** generate minimal IR fixtures directly. Rejected because they would bypass precisely the source semantics, specialization, closure conversion, ownership, and backend paths under investigation.

### 2. Inspect four compilation boundaries

The harness records normalized artifacts for each pair:

1. HIR, to prove `|>` is already ordinary application.
2. MIR, to locate wrappers, runner calls, state tags, branches, environments, and allocations introduced before backend optimization.
3. Unoptimized and optimized LLVM IR plus native assembly and code size, to determine what LLVM removes.
4. Direct-Wasm WAT and binary size, to independently evaluate the backend that bypasses LLVM.

Artifact normalization removes unstable names, addresses, and unrelated metadata while preserving control flow and operations relevant to the comparison. The harness records compiler configuration and uses fresh compiler processes so cached state does not distort results.

**Alternative considered:** inspect only optimized assembly or benchmark timings. Rejected because final output alone cannot identify which compiler layer introduced or removed a cost, and timings at this scale are especially noisy.

### 3. Treat structural equivalence as the primary result

For a statically known, non-escaping, non-suspending pipeline in a release build, the Effect version succeeds when its final output has the same essential operations and control-flow shape as the baseline and retains none of the following solely because of Effect composition:

- completed-versus-suspended tags or suspension branches;
- heap allocation;
- an indirect runner call when the runner target is statically known;
- materialized intermediate Effect wrappers or closure environments.

Exact textual IR equality is not required. Code size is supporting evidence. Runtime microbenchmarks may be added only when the fixture does enough work to measure reliably and the harness reports distributions rather than a single timing.

Because suspension is not expressible yet, this spike does not invent a fake continuation control. The report instead records the absence of suspension representation as a measured baseline. Once suspension is implemented, the corpus MUST gain both a genuinely suspending control whose continuation survives and a proven non-suspending case that remains comparable with this baseline.

**Alternative considered:** define a percentage runtime threshold. Rejected because it would be hardware-sensitive and would not tell us whether a representation cost exists.

### 4. Analyze semantic safety alongside cost

Every proposed equivalence is checked for preserved evaluation order, failure short-circuiting, traps, ownership transfer, and exactly-once `Drop` behavior. A transformation that improves the generated artifact but changes these semantics is classified as invalid, not as a successful optimization.

**Alternative considered:** limit the spike to pure `Copy` values. Rejected because a normalization that only works by ignoring affine cleanup would provide misleading architectural confidence.

### 5. Use a predetermined decision tree

The report selects exactly one recommendation:

1. **Backend optimization is sufficient.** Native LLVM and direct Wasm both meet the structural criteria. Document the evidence and add regression fixtures; do not add an optimizer.
2. **Shared MIR normalization is required.** One backend retains avoidable composition machinery, or both do, while the compiler can prove the relevant callbacks and runners are non-escaping and non-suspending. Propose a backend-independent MIR normalization with explicit semantic guards.
3. **A synchronous runner specialization is likely required later.** This outcome cannot be selected from the current corpus because no complete-or-suspended representation exists yet. Record the applicability criteria for a future rerun rather than proposing an ABI from synthetic evidence.

If different corpus cases support different outcomes, choose the least invasive mechanism that covers the proven static subset and explicitly leave dynamic cases unspecialized.

**Alternative considered:** implement a source-level or pipe-specific rewrite immediately. Rejected because pipe syntax is already erased and such a pass would optimize spelling rather than semantics.

### 6. Keep the spike reproducible but non-normative

Fixtures and structural assertions live with compiler tests or test support. The final analysis is recorded under `wayfinder/bootstrap-language/research/synchronous-effect-cost-spike.md`, including commands, toolchain versions, artifact references, findings, and the selected recommendation. Raw transient build products need not be committed when the harness can reproduce them deterministically.

The report informs later architecture but does not itself change a language specification. Any optimizer, ABI, or semantic change requires its own OpenSpec proposal.

## Risks / Trade-offs

- **[The library-definable Effect representation changes after the spike]** → Run only after `make-effects-library-definable` reaches a representative implementation, and record the measured commit and compiler configuration.
- **[Tiny fixtures produce misleadingly perfect optimization]** → Include named helpers, stored Effect values, requirement operations, and affine ownership in addition to direct inlinable chains.
- **[Artifact snapshots become brittle across LLVM versions]** → Assert semantic structural properties and normalize incidental metadata; keep full artifacts available for manual inspection without requiring exact-text snapshots.
- **[Direct-Wasm and LLVM optimize different subsets]** → Analyze them independently and prefer a shared MIR solution only when evidence demonstrates a cross-backend gap.
- **[A non-suspending proof becomes unsound as suspension is introduced]** → Record that this corpus covers only the current synchronous runtime and require a genuine suspending control plus conservative invalidation and fallback behavior when suspension is introduced.
- **[The spike expands into optimizer implementation]** → Stop at the report and recommendation; track implementation only through a follow-up proposal.

## Migration Plan

1. Complete enough of `make-effects-library-definable` to expose the representative runner core and source combinators.
2. Land the corpus and reproducible artifact-capture harness without changing compiler semantics.
3. Capture and review results for both native LLVM and direct Wasm release configurations.
4. Publish the Wayfinder report and add stable structural regression checks for observations that should remain true.
5. If outcome 2 or 3 is selected, create a separate OpenSpec proposal before changing MIR or the runner ABI. If outcome 1 is selected, close the spike with no optimizer work.

Rollback consists of removing the research-only fixtures and harness additions; no user-facing behavior or stored data requires migration.
