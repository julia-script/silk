## Why

JUL-117 completes stage 2 of the accepted [Lifetimes in Silk](https://linear.app/juliaortiz/document/lifetimes-in-silk-769ef53bffd0) design. Shared borrowed aggregates are insufficient for mutable views and owning containers whose payloads and destructors retain references.

## What Changes

- Admit affine exclusive fields and conservative dependent Drop after validating their complete ownership, variance, cleanup and escape obligations.
- Preserve reborrow ancestry through stored values, extraction and shared-child copies; check replacement against unchanged destination types.
- Validate RawBuffer/Slot as the unsafe storage boundary and prove borrowed shared and exclusive elements in the maintained Vector implementation, including growth, failure, extraction and initialized-range destruction.
- Keep dependent Effect outcomes and partial-owner suspension gated for JUL-118. No Box factory integration, pinning, arena family or new compiler-known collection is introduced.
- Reconcile reference, diagnostics, fixtures and opt-in work measurements with the new admission boundary.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `bootstrap-lifetimes`: exclusive storage, dependent cleanup, declaration summaries and erased generic reuse.
- `bootstrap-ownership`: reborrow chains, complete-field transfers and replacement cleanup obligations.
- `bootstrap-owned-allocation`: dependent typed storage and conservative Drop.
- `bootstrap-intrinsic-boundary`: validated raw-storage lifetime, variance and provenance contracts.
- `bootstrap-type-generics`: borrowed payloads in invariant storage without lifetime specialization.
- `bootstrap-silk-stdlib`: ordinary-source Vector witness for shared and affine borrowed elements.

## Impact

Compiler lifetime admission/flow, ownership, type compatibility, intrinsic typing and cleanup; RawBuffer, Slot and Vector; prescriptive reference and generated diagnostics; existing semantic/MIR tests, native acceptance corpus and lifetime benchmark workloads. JUL-116 is present in the base and marked Done in Linear. Its deferred independent review is not an implementation dependency.
