## Context

The semantic prerequisite provides exact body/endpoint properties, suspension summaries, affine
Execution identity, and lifecycle states. Current Layout and Allocation already encode target-aware
size/alignment and self-contained reclaim authority. See `proposal.md` and the five delta specs.

## Goals / Non-Goals

**Goals:**

- Package every fixed execution value and initial storage in one recoverably procured Allocation.
- Cross one purpose-bound erasure boundary while retaining exact invoke and cleanup metadata.
- Give source owners one allocation-free callback-shaped drive protocol and execution-local stack.

**Non-Goals:**

- Define Wake ordering, Scheduler policy, canonical wrappers, implicit roots, or recoverable dynamic
  continuation growth.
- Expose a stable package layout, public frame representation, or general callable boxing.

## Decisions

### Use one exact combined package

Extend target layout with a package plan keyed by `A`, `F`, `O`, `R`, target, and suspension
summary. Store the erased body and endpoint values with exact hidden invoke/drop metadata and reserve
wake/initial-segment lanes only when statically required. `executionFromAllocation` validates the
plan identity and consumes the complete Allocation in one MIR transition.

Alternatives rejected: separate execution and readiness packages add an allocation, failure branch,
and reclaim protocol; runtime allocation hides admission failure; universal layout wastes target-
and specialization-specific space.

### Keep endpoint state separate from its reusable callback

The package stores detached `O` plus reusable exact `R(&O)`. This permits affine routing state to be
owned once and borrowed across multiple notifications. A capturing reusable callback cannot own
affine state under the current callable model, while a consuming callback cannot be reused.

Alternatives rejected: per-drive callback erasure broadens drive and allocates/retargets readiness;
one capturing `fn()` needs a broader callable capability.

### Lower drive as one consuming branch transfer

Verified MIR consumes Execution, `D`, `C`, and `S`. One terminal edge transfers `D,A` to `C`; the
external-park edge transfers `D,Execution` to `S`; each edge drops the other callback. Nested
transfers remain inside the running execution and do not branch to the owner.

Alternatives rejected: a sealed step-result sum adds a compiler-owned data type; duplicating branch
captures requires Clone or source sharing; returning Execution directly cannot express completion.

### Root one logical stack per Execution

At first drive create an execution-relative logical stack context. Save it with the owner record on
relinquishment and restore it on later drives. Owner-side drive machinery is a control boundary, not
a logical caller. Native/Wasm physical placement remains for the engine slice.

Alternatives rejected: using one thread-global LIFO context makes non-LIFO owner scheduling change
CallDepth and trace ancestry.

### Keep package admission recoverable and later growth fatal

The safe source wrapper procures one Allocation through ordinary policy before unsafe construction.
Afterward, continuation-stack growth reuses the existing fatal execution-stack contract. No hidden
Allocator provider or failure row crosses into Intrinsic.

Alternatives rejected: fatal initial packaging loses bounded admission/rollback; recoverable later
growth changes existing suspension semantics and complicates every Effect outcome.

## Risks / Trade-offs

- **A cancelled Wake can later retain the entire package** → accept the indivisible-ticket cost and
  make the parking slice verify final authority accounting.
- **Layout identity drifts from initializer specialization** → carry one canonical package-plan
  digest into semantic facts and verified MIR and reject mismatches before lowering.
- **Erased cleanup misses a concrete capture** → derive invoke/drop metadata from the same exact
  environment used by Detached and compare ownership/MIR goldens.
- **Owner callbacks accidentally park** → require NonParking statically and keep drive callbacks
  nonescaping and take-once.
