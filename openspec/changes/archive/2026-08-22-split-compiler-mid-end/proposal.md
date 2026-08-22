## Why

`Lower.ts` is an 8,581-line god-module mixing ~10 lowering concerns with a 2,300-line per-expression switch; its provider-loan and end-loan choreography is duplicated four-to-five times and has already diverged on when loans end. `Ownership.ts` (3,453 lines) embeds a test-facing codec and a self-contained CleanupPlan sub-concept. `SuspensionOwnership.ts` duplicates `Ownership`'s release-order and set logic.

## What Changes

- **Split `Lower.ts`** into `Forwarding`, `ValueType`, `EffectLowering`, `WitnessLowering`, `LowerExpression` (+ `LowerBuiltin` for the intrinsic chain), `CleanupEmission`, `LowerStatements`, and `EntryAssembly`, driven by a shared `FunctionLowering` state module.
- **Deduplicate provider-loan lowering** into one `lowerProvidedEffect` and the five inline end-loan loops into `endLoans`/`endRunLoans`.
- **Extract `Ownership`'s CleanupPlan** sub-concept into `CleanupPlan.ts` and its deterministic codec into `OwnershipEncoding.ts`.
- **Share `inReleaseOrder`** across `Ownership`/`SuspensionOwnership`/`Lower` and route `SuspensionOwnership` through `Ownership.cleanupPlan` (already partially done).

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

<!-- none: behavior-preserving refactor (skip_specs) -->

## Impact

Pure refactor of the HIR→MIR middle end. Borrow identities, loan-end ordering, and release ordering must stay byte-identical; golden/determinism suites are the net. `skip_specs: true`.
