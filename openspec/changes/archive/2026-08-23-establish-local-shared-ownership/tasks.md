## 1. Canonical Semantic Model

- [x] 1.1 Add the sealed generic `Intrinsic.SharedCore<T>` type identity and publish its element,
      exact `LocalSharedStrong` role, affine category, and `LocalExecution` affinity; verify semantic
      fact tests encode those fields without representation lanes.
- [x] 1.2 Add the closed execution-affinity property and recursive deterministic join across nominals,
      arrays, normalized unions, references, borrowed views, callable environments, Effect
      environments, and unavailable components; verify focused semantic tests cover
      all-unrestricted, local-plus-unrestricted, parameter-dependent, unavailable-plus-local, and
      multiple-unavailable inputs and retain all distinct causes in canonical traversal order.
- [x] 1.3 Preserve affinity and intrinsic identity through generic specialization; verify one generic
      wrapper over Copy and affine element types retains the canonical identity, exact role, local
      affinity, canonical element, and one affine handle obligation in both cases.
- [x] 1.4 Publish `ParameterDependent` for an unspecialized generic aggregate and re-normalize after
      unrestricted, local-core, and unavailable substitutions; verify canonical parameter identity,
      concrete outcomes, and unavailable causes without fabricating a diagnostic for the open form.
- [x] 1.5 Propagate affinity from the exposed type and canonical borrow-root dependency through shared
      and exclusive references, borrowed views, and executable captures; verify no projection or
      capture erases `LocalExecution`.
- [x] 1.6 Publish the deterministic affinity outcome through semantic inspection for a core and
      recursively containing value; verify no transfer syntax, eligibility branch, dormant
      diagnostic, local-provenance representation, execution/fiber/Scheduler identity, or
      MIR/backend transfer check is introduced by this slice.

## 2. Ownership Integration

- [x] 2.1 Classify every available local shared core as affine independently of `T`; verify ownership
      tests accept one whole-handle move and report `OWN0003` with the attempted-read span for a
      non-consuming duplication while retaining the affine fact and no duplicate obligation.
- [x] 2.2 Reject `impl Copy` for a nominal containing a local shared core during conformance validation;
      verify `SEM0083` points at the implementation declaration, no Copy witness is published, and
      ownership receives the nominal as affine rather than owning this diagnostic.
- [x] 2.3 Propagate handle obligations and local affinity through nominal, fixed-array, active-union,
      callable, and Effect storage; verify one stored handle yields one obligation, two independently
      stored handles yield two distinct obligations, only an active union member contributes
      obligations, moved sources end, and no operation on `T` is planned.
- [x] 2.4 Retain one obligation and `LocalExecution` affinity through suspension, parking, resumption,
      and a move between independently resumable frames in one same-thread local execution domain;
      verify the moved source ends, the destination owns exactly one obligation, park/resume changes
      no obligation, and no Scheduler policy or later-slice lifecycle count/drop behavior is added.
- [x] 2.5 Retain explicit unavailable ownership and affinity facts under recovery; verify
      `Intrinsic.SharedCore<Missing>` preserves the element-resolution diagnostic and fabricates no
      Copy category, unrestricted affinity, satisfied verdict, or live obligation.

## 3. Privilege and Determinism Gates

- [x] 3.1 Add ordinary source declarations named `Shared`, `SharedCore`, `Deferred`, `Scheduler`, and
      `LocalRuntimeHandle` to semantic and ownership tests; verify every declaration remains an
      available ordinary nominal and none receives intrinsic identity, the exact `LocalSharedStrong`
      role, spelling-derived `LocalExecution` affinity, or a live local-shared obligation.
- [x] 3.2 Gate the new semantic and ownership encodings with committed deterministic goldens and verify
      repeated in-process analysis produces byte-identical output without adding a redundant
      fresh-process determinism test.
- [x] 3.3 Run focused semantic, conformance, ownership, recovery, and inspection tests, then
      `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and
      `pnpm release:candidate`; record each exact result and identify any pre-existing failure before
      handing this prerequisite to the allocation slice.
