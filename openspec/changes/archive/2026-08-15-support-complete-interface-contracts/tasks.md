## 1. Contract Facts

- [x] 1.1 Confirm conditional generic conformances are complete and representation parameters are available for witness binders.
- [x] 1.2 Extend interface declarations, applications, and mapped operations with literal operand shapes, flow kind, success, failure rows, requirement rows, and access.
- [x] 1.3 Add syntax, kind, provider-equality, damaged, and visibility fixtures for complete contracts.

## 2. Literal Ownership and Subsumption

- [x] 2.1 Replace general value-to-shared-borrow witness adaptation with literal source operand matching.
- [x] 2.2 Implement receiver/parameter access compatibility and deterministic stronger-demand diagnostics.
- [x] 2.3 Implement failure-row, requirement-row, and access subsumption while preserving exact generic caller contracts.
- [x] 2.4 Add pure/smaller witness positives and stronger-row/access negatives.
  - PR2 lands the pure compatibility actor and focused evidence only. Conformance admission remains
    on the legacy checker until literal operand lowering and pure/effect boundary widening can land
    atomically with the ownership migration.
  - PR5 closes the remaining acceptance ledger with permanent source/runtime coverage for a
    fallible `&mut`-to-`&` witness reborrow, requirement-row access widening, rejection of a
    borrowed witness for a value-owned contract, and failing generic smaller-row propagation.

## 3. Generic Witness Targets

- [x] 3.1 Infer mapped target type, row, and representation binders from substituted conformance and operation contracts.
- [x] 3.2 Preserve generic target-inference inputs in unresolved HIR witness questions and inferred
  target arguments in conformance mappings and concrete instance keys.
- [x] 3.3 Add two-specialization acceptance plus unresolved and conflicting binder diagnostics.
- [x] 3.4 Lower every admitted witness to one static target with no runtime dictionary or service slot.

## 4. Interface Migration

- [x] 4.1 Rewrite ordinary `Order` and `HashKey` interface operands and witnesses with explicit intended borrows.
- [x] 4.2 Confine any transitional operand adapter to sealed intrinsic witness lowering.
- [x] 4.3 Update standard-library, operator, collection, and compiler acceptance fixtures for the breaking ownership convention.

## 5. Verification

- [x] 5.1 Run `pnpm typecheck` and `pnpm exec biome check .`.
- [x] 5.2 Run `pnpm test`, `pnpm check`, and `pnpm release:candidate`; report exact failures.
- [x] 5.3 Inspect specialized HIR/MIR to confirm caller rows stay exact while dead witness machinery may optimize away.

## Final Evidence

- Contract facts (1.1–1.3): conditional conformance and representation work is present in merged
  changes `f472606` and `df97fe2`; `CompleteInterfaceContractsFixtures.test.ts` verifies complete
  declaration/application facts, provider equality, damaged and visibility cases, formatting, and
  representation binders.
- Ownership and subsumption (2.1–2.4): literal operand compatibility and the only permitted
  exclusive-to-shared weakening are exercised by `BoundOperationWitness.test.ts` and
  `InterfaceWitnessCompatibility.test.ts`. The PR5 cases additionally execute combined access/row
  weakening and exact failure propagation on bootstrap, direct Wasm, and native LLVM, assert exact
  HIR requirement rows, and pin the source-level ownership diagnostic.
- Generic witness targets (3.1–3.4): `InterfaceWitnessInference.test.ts` and
  `ConditionalConformance.test.ts` cover declaration-order inference, unresolved/conflicting
  diagnostics, unresolved HIR question inputs, mapping/instance target arguments, two concrete
  specializations, direct static targets, and typed-failure runtime propagation.
- Interface migration (4.1–4.3): merge `0acd894` migrates `Order`, `HashKey`, their standard-library
  consumers, and compiler fixtures to literal borrows. The transitional adapter remains confined to
  sealed intrinsic admission/HIR/lowering and is fenced by `BoundOperationWitness.test.ts`.
- Verification (5.1): `pnpm typecheck` passed (20/20 tasks). `pnpm exec biome check .` passed over
  719 files with one non-failing pre-existing `useLiteralKeys` informational finding in
  `packages/compiler/scripts/vendor-unicode-data.mjs`.
- Verification (5.2): `pnpm test` and `pnpm check` were run and each reached the same pre-existing,
  host-stack-sensitive clean-main failure at `WasmShadowStackHeapCollision.test.ts:430`: expected
  `0`, received `undefined`; the compiler suite otherwise passed 1,828/1,829 tests. The failure also
  reproduces in isolation and this change has no compiler implementation diff. `pnpm
  release:candidate` passed (7/7 release validation tests).
- Verification (5.3): the focused compiler batch passed 36/36 tests. HIR retains exact failure and
  requirement rows on the generic bound-operation contract. Raw MIR retains the exact
  `EffectOutcome` and a direct call to the specialized witness target with no dictionary, vtable,
  interface service call, or service slot. Normalized MIR either retains that runner or a
  `RunStaticEffect` with the same exact outcome row, while pure witness failure machinery may be
  removed.

## Pressure-Debt Disposition

- Closed by PR5 acceptance tests: combined fallible Effect access weakening; requirement-row/access
  weakening; value-contract-to-borrowed-witness rejection; and failing generic smaller-row runtime
  propagation.
- Carried forward as future cleanup only: remove the sealed intrinsic operand bridge when the
  remaining compiler-sealed witnesses migrate. Until then it remains permitted only under the
  active specification's sealed intrinsic admission, HIR, and lowering fences; it is not available
  to ordinary interface witnesses and does not weaken literal ownership.
