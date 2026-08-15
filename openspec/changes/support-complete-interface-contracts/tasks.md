## 1. Contract Facts

- [x] 1.1 Confirm conditional generic conformances are complete and representation parameters are available for witness binders.
- [x] 1.2 Extend interface declarations, applications, and mapped operations with literal operand shapes, flow kind, success, failure rows, requirement rows, and access.
- [x] 1.3 Add syntax, kind, provider-equality, damaged, and visibility fixtures for complete contracts.

## 2. Literal Ownership and Subsumption

- [ ] 2.1 Replace general value-to-shared-borrow witness adaptation with literal source operand matching.
  - Deferred in PR2: removing the legacy value adapter depends on the atomic `Order`/`HashKey`
    ownership-convention migration in tasks 4.1–4.3; mixed source conventions are not mergeable.
- [x] 2.2 Implement receiver/parameter access compatibility and deterministic stronger-demand diagnostics.
- [x] 2.3 Implement failure-row, requirement-row, and access subsumption while preserving exact generic caller contracts.
- [x] 2.4 Add pure/smaller witness positives and stronger-row/access negatives.
  - PR2 lands the pure compatibility actor and focused evidence only. Conformance admission remains
    on the legacy checker until literal operand lowering and pure/effect boundary widening can land
    atomically with the ownership migration.

## 3. Generic Witness Targets

- [x] 3.1 Infer mapped target type, row, and representation binders from substituted conformance and operation contracts.
- [x] 3.2 Preserve inferred target arguments in HIR witness questions and concrete instance keys.
- [x] 3.3 Add two-specialization acceptance plus unresolved and conflicting binder diagnostics.
- [x] 3.4 Lower every admitted witness to one static target with no runtime dictionary or service slot.

## 4. Interface Migration

- [ ] 4.1 Rewrite ordinary `Order` and `HashKey` interface operands and witnesses with explicit intended borrows.
- [ ] 4.2 Confine any transitional operand adapter to sealed intrinsic witness lowering.
- [ ] 4.3 Update standard-library, operator, collection, and compiler acceptance fixtures for the breaking ownership convention.

## 5. Verification

- [ ] 5.1 Run `pnpm typecheck` and `pnpm exec biome check .`.
- [ ] 5.2 Run `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact failures.
- [ ] 5.3 Inspect specialized HIR/MIR to confirm caller rows stay exact while dead witness machinery may optimize away.
