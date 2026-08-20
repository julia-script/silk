## Why

Branches and other finite joins can currently reject compatible Effects solely because they have different construction identities. The stabilized language treats concrete Effect identity as hidden implementation detail and requires compatible success, failure, requirement, and access contracts to join without an allocation or runtime type erasure.

## What Changes

- Replace construction-identity rejection with a finite composite Effect realization when branch contracts are compatible.
- Compute joined success, ordinary failure unions, requirement unions, and capture access before specialization.
- Preserve laziness, one-layer `run`, ownership, cleanup, and exact selected branch execution.
- Emit a deterministic allocation-free composite in HIR/MIR, evaluation, LLVM, and Wasm.
- Retain precise diagnostics for genuinely incompatible joins and non-finite representation demands.

## Capabilities

### Modified Capabilities

- `bootstrap-flow-functions`: admit finite compatible Effect joins independently of construction identity.
- `bootstrap-hir`: represent a resolved finite Effect composite.
- `bootstrap-mir`: lower and verify only the selected branch while preserving joined contracts.
- `bootstrap-evaluation`: execute the selected lazy Effect exactly once.
- `bootstrap-backend`: realize the composite deterministically without heap allocation.

## Impact

Depends on `normalize-effect-failure-types` and `normalize-effect-requirement-provision`. It removes the relevant `SEM0069` behavior and updates representation, ownership, evaluation, both backends, diagnostics, and tests. It does not introduce arbitrary runtime polymorphism.
