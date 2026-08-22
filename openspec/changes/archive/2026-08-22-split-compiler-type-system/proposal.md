## Why

Three front/middle-end modules mix several concerns and, in `Pipeline`'s case, duplicate an entire lowering pipeline. `Type.ts` (3,367 lines) carries the full constraint-based generic-inference engine inside the data module; `Instances.ts` (3,297 lines) mixes instance discovery, diagnostic-violation checks, executable-origin recovery, and suspension analysis in one closure; `Pipeline.ts` (887 lines) hosts three interleaved lifecycles plus two near-identical `realize`/`prepare` lowering pipelines that are already drifting.

## What Changes

- **Extract `TypeInference.ts`** from `Type.ts` (lines 2806–3367); inference callers import the owning actor directly and `Type` retains no forwarding exports.
- **Split `Instances.ts`** into `Instances` (discovery worklist + `specialize`/`keyOf`), `InstanceDiagnostics` (the five `*Violations` + `storedRepresentation`/`representedNominals`), and `ExecutableOrigin` (`*OriginOf`, `concreteCallables`/`concreteEffects`, `suspensionGraph`, reachability/witness targets).
- **Split `Pipeline.ts`** into `Frontend`, an incremental-reuse module, and `Realization`; extract one shared `discoverAndLower` so `realize`/`prepare` become thin gate/error mappers instead of duplicate pipelines.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

No observable behavior change. Touches `Type.ts`, `Instances.ts`, `Pipeline.ts`, and their callers; the split actors are direct internal imports rather than compatibility facades. `realize`/`prepare` must keep their existing gate/error semantics byte-for-byte (only the shared skeleton is de-duplicated). `skip_specs: true`.
