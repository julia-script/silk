## Why

JUL-118 completes stage 3 of the accepted [Lifetimes in Silk](https://linear.app/juliaortiz/document/lifetimes-in-silk-769ef53bffd0) design. The merged lifetime and exclusive-storage foundations preserve borrowed data but still reject dependent Effect outcomes and suspension with partial owners, preventing ordinary borrowing stream operations and Box.make with borrowed elements.

## What Changes

- Admit lifetime-bearing successes and failures only with verified propagation, cleanup, environment and stable-storage obligations; preserve external result dependencies independently of temporary receiver loans.
- Preserve `Effect<'env; A ! E ? R>` and `effect<'env> fn` contracts through provision, callable results, generic composition and storage, including exact representation and shared/exclusive/consuming access.
- Extend existing suspended-state ownership to definite, missing and conditional partial components, resumption, cancellation and stable referents.
- Complete expected-type-directed bounded quantified callable and operation compatibility with fresh invocation receiver lifetimes.
- Prove fixed-item borrowing operations and existing Box.make in ordinary source; preserve Copy and affine payload dependencies without introducing a compiler-known Stream.
- Keep exact environment detachment independent of outcome bounds, affinity and NonParking.
- Reconcile reference, diagnostics and fixtures and extend opt-in query/work workloads with success and failure cases.

## Capabilities

### New Capabilities

- `bootstrap-fixed-item-iteration`: ordinary-source compiler witnesses for fixed lifetime-bearing items; public Stream and buffering design remains JUL-21.

### Modified Capabilities

- `bootstrap-lifetimes`: lifetime-bearing outcomes, stable-source admission and finite query reuse.
- `bootstrap-ownership`: outcome escape/cleanup and partial suspended owner state.
- `bootstrap-callable-values`: environment transport and quantified invocation compatibility.
- `bootstrap-independent-execution-semantics`: independent environment and completion bounds.
- `bootstrap-nominal-effect-storage`: stored Effect environment and complete outcome preservation, including existing owned containers.

## Impact

Compiler declaration/admission facts, callable compatibility, Effect/catch/run ownership, provider capture, suspension planning and native frame lowering; semantic, MIR and shared native acceptance evidence; opt-in lifetime workloads; prescriptive ownership, failure and suspension reference. JUL-117 is merged at checkout baseline ed4b8433. No new library API, backend, execution classifier, optional-provider policy, runtime lifetime identity, lending item family, pinning or structured concurrent caller borrowing is introduced. Fatal traps retain their existing no-unwind contract.
