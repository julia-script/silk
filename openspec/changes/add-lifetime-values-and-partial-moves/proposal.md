## Why

Silk's one-source returned-view restriction prevents ordinary generic holders of borrowed data, and whole-owner move tracking prevents extracting a field while retaining its siblings. JUL-116 implements the accepted [Lifetimes in Silk](https://linear.app/juliaortiz/document/lifetimes-in-silk-769ef53bffd0) design as a complete shared-borrow capability with predictable checking and inspectable contracts.

## What Changes

- **BREAKING** Replace position-restricted returned-view admission with named semantic lifetimes, declaration-only elision, ordinary generic propagation, finite outlives/variance checking and place-based loans.
- **BREAKING** Replace whole-root initialization with sparse move paths, branch/loop joins, verified restoration and conditional remainder cleanup. Admit visible owned fields, constant fixed-array indices and refined active payloads; preserve Drop and dereference boundaries.
- Preserve callable/Effect environment validity and nested data lifetimes through abstraction from the first increment. Keep exclusive stored views, dependent user Drop, borrowed Effect outcomes and suspended partial owners gated until JUL-117/JUL-118 supply their proofs.
- Encode declaration-relative lifetime binders and semantic dependency summaries; reuse checked generic bodies and comparisons; erase lifetime arguments at runtime identity/layout boundaries.
- Provide stable lifetime expansion in formatting/editor APIs, useful diagnostic witnesses, structural reuse/erasure checks and opt-in growth workloads.

## Capabilities

### New Capabilities

- `bootstrap-lifetimes`: Named and inferred regions, variance, quantification, use-driven obligations and phase/work-accounting contracts.

### Modified Capabilities

- `bootstrap-syntax`: Lifetime binders/arguments, bounds, bounded quantified callables and owned-place variant refinement.
- `bootstrap-ownership`: Sparse partial initialization, retained loans, restoration, cleanup and sound staged admission.
- `bootstrap-runtime-slices`: Lifetime-bearing stored views and declared multiple-source ordinary results.
- `bootstrap-type-generics`: Lifetime-preserving generic checking and erased runtime arguments.
- `bootstrap-semantic-facts`: Canonical lifetime relationships and place-state evidence.
- `bootstrap-callable-values`: Environment bounds and expected-type-directed finite quantification.
- `bootstrap-module-semantic-surface`: Declaration summaries and actual-consumer semantic invalidation.
- `silk-source-formatting`: Canonical lifetime syntax and readable expansion.
- `language-server-code-actions`: Make inferred lifetimes explicit.
- `language-server-hover`: Show inferred lifetime relationships.
- `bootstrap-fixed-arrays`: Sparse constant-index ownership and borrowed elements.
- `bootstrap-struct-values`: Field extraction and stored borrowed payloads.
- `bootstrap-nominal-effect-storage`: Lifetime-preserving captured storage admission.
- `bootstrap-nominal-callable-storage`: Lifetime-preserving callable storage admission.
- `bootstrap-hir`: Explicit partial ownership and consuming projected transfers.
- `bootstrap-structural-unions`: Lifetime-bearing union alternatives without detached erasure.

## Impact

Owns [JUL-116](https://linear.app/juliaortiz/issue/JUL-116), proposal stages 0/1/4. Changes compiler syntax, Type, declaration completion, compatibility, ownership, cleanup, HIR/MIR transport, query reuse, instance identity and editor presentation, plus reference, diagnostics and fixtures. No new library actor receives compiler privilege. JUL-117 extends exclusive storage/dependent Drop and JUL-118 extends outcomes/frames on subsequent `gh stack` branches; JUL-21 retains public Stream library ownership. Verification uses focused semantic/MIR checks during implementation and the required repository checks at the substantial handoff milestone.
