## 1. Preserve the Exact Reachable Runner

- [x] 1.1 Keep the minimized non-TLS `Suspendability` regression for an owned `Option<P>` provider
      selected inside a generic `useReleaseNonParking` callback, and verify semantic diagnostics are
      empty while the test directly requires clean final MIR verification.
- [x] 1.2 Trace the callback's canonical result-Effect identity through executable discovery,
      provisional materialization, layout, and final lowering; repair the first missing structural
      forwarded-provider edge, and verify the regression retains one matching final MIR runner with
      its exact provider witness, call shape, and cleanup dependencies.
- [x] 1.3 Keep the affected executable/provided-runner worklists unchanged and canonical, and verify
      the repaired module retains the runner exactly once without retaining an unreachable provider
      specialization.

## 2. Lock the Boundary with Structured Evidence

- [x] 2.1 Add or reuse discriminating structured controls showing direct-field generic provision,
      nominal-union selection without a provided call, and the existing borrowed generic provider
      bracket remain accepted, while a deliberately missing or inconsistent runner still produces
      `InvalidEffectOperation`.
- [x] 2.2 Keep the implementation target-neutral and structural, with focused assertions proving it
      adds no actor/name recognition, new intrinsic, backend-specific reconstruction, universal
      Effect dispatch, forced suspension classification, or catch-all runner retention.
