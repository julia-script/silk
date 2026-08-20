# Stabilized Silk core completion audit

Status: complete
Audited against: repository state after `6bab8a8`

## Scope and method

This is the post-implementation audit for the 16 dependency-ordered changes in
[`map.md`](map.md). The earlier reconciliation map is the historical baseline that discovered the
gaps; this document records the evidence that closed them.

For every queue item, the audit checked that:

1. the dated archive contains `.openspec.yaml`, proposal, delta specs, design, and tasks;
2. every task is checked;
3. every added or modified requirement and named scenario is present in the final canonical spec,
   every removed requirement is absent, and both renamed requirements have only their new name;
4. implementation and focused regression tests exist for the promised boundary; and
5. the complete repository and release gates pass after the dependency-ordered sequence.

Across the roadmap, the archives contain 186 completed tasks, 136 delta requirements, and 289
named scenarios. The final semantic comparison found one scenario that a later full requirement
edit had accidentally omitted—whole-failure `catchAll` recovery—and restored it in `6bab8a8`.
Repeating the comparison then reported zero synchronization differences.

## Item-by-item evidence

| # | Closed plan | Delta / tasks | Implementation evidence | Focused regression evidence |
| --- | --- | --- | --- | --- |
| 01 | [Return-contract soundness](../../openspec/changes/archive/2026-08-19-enforce-return-contract-soundness/proposal.md) | 5 requirements, 8 scenarios, 11/11 tasks | `3aeb734`, `7d622ff` | [`Elaboration.test.ts`](../../packages/compiler/test/Elaboration.test.ts), [`InterfaceBounds.test.ts`](../../packages/compiler/test/InterfaceBounds.test.ts) |
| 02 | [Ordinary Effect failure types](../../openspec/changes/archive/2026-08-19-normalize-effect-failure-types/proposal.md) | 12 requirements, 20 scenarios, 12/12 tasks | `7c348b9`–`3a4de74`; final sync repair `6bab8a8` | [`SelectiveCatch.test.ts`](../../packages/compiler/test/SelectiveCatch.test.ts), [`TypeGenerics.test.ts`](../../packages/compiler/test/TypeGenerics.test.ts) |
| 03 | [Unified interface/service conformance](../../openspec/changes/archive/2026-08-20-unify-interface-service-conformance/proposal.md) | 8 requirements, 22 scenarios, 11/11 tasks | `eb92923`, `2acd9f7`, `a0abece` | [`UserInterfaceWitness.test.ts`](../../packages/compiler/test/UserInterfaceWitness.test.ts), [`UserServices.test.ts`](../../packages/compiler/test/UserServices.test.ts) |
| 04 | [Requirement keys and provision](../../openspec/changes/archive/2026-08-20-normalize-effect-requirement-provision/proposal.md) | 5 requirements, 12 scenarios, 12/12 tasks | `507f189`, `23b570f`, `536e657` | [`ProviderSelection.test.ts`](../../packages/compiler/test/ProviderSelection.test.ts), [`ProvideEffectAcceptance.test.ts`](../../packages/compiler/test/ProvideEffectAcceptance.test.ts) |
| 05 | [Entry termination and reporting](../../openspec/changes/archive/2026-08-20-align-entry-termination-reporting/proposal.md) | 10 requirements, 14 scenarios, 13/13 tasks | `0e2e950`, `e028364`, `64fa4b5`, `e4e7945` | [`EffectEntry.test.ts`](../../packages/compiler/test/EffectEntry.test.ts), [`Report.test.ts`](../../packages/compiler-cli/test/Report.test.ts) |
| 06 | [Compatible Effect joins](../../openspec/changes/archive/2026-08-20-admit-compatible-effect-joins/proposal.md) | 6 requirements, 14 scenarios, 11/11 tasks | `5c6dec8`, `47b31dd` | [`EffectJoin.test.ts`](../../packages/compiler/test/EffectJoin.test.ts) |
| 07 | [Copy and executable ownership](../../openspec/changes/archive/2026-08-20-define-copy-and-executable-ownership/proposal.md) | 11 requirements, 27 scenarios, 12/12 tasks | `edeb813`, `ab43366` | [`Ownership.test.ts`](../../packages/compiler/test/Ownership.test.ts), [`StoredCallableOwnership.test.ts`](../../packages/compiler/test/StoredCallableOwnership.test.ts) |
| 08 | [Borrows and callable lifetimes](../../openspec/changes/archive/2026-08-20-generalize-borrows-and-callable-lifetimes/proposal.md) | 18 requirements, 25 scenarios, 12/12 tasks | `8ed0fdc`–`86a276d` | [`QualifiedBorrowArgument.test.ts`](../../packages/compiler/test/QualifiedBorrowArgument.test.ts), [`RuntimeSliceOwnership.test.ts`](../../packages/compiler/test/RuntimeSliceOwnership.test.ts) |
| 09 | [Ordinary structural unions](../../openspec/changes/archive/2026-08-20-generalize-ordinary-structural-unions/proposal.md) | 15 requirements, 44 scenarios, 12/12 tasks | `2ae9031`, `b58f5bd` | [`StructuralUnionRuntime.test.ts`](../../packages/compiler/test/StructuralUnionRuntime.test.ts), [`StructuralUnionSyntax.test.ts`](../../packages/compiler/test/StructuralUnionSyntax.test.ts) |
| 10 | [Struct construction and inference](../../openspec/changes/archive/2026-08-20-complete-struct-construction-and-inference/proposal.md) | 6 requirements, 19 scenarios, 10/10 tasks | `cb00392`, `1daaae3`, `5facbfb` | [`StructValues.test.ts`](../../packages/compiler/test/StructValues.test.ts), [`InlineStructReach.test.ts`](../../packages/compiler/test/InlineStructReach.test.ts) |
| 11 | [Text and scalar values](../../openspec/changes/archive/2026-08-20-normalize-text-and-scalar-values/proposal.md) | 7 requirements, 24 scenarios, 10/10 tasks | `844a9ed`, `49d81cb` | [`CharacterScalar.test.ts`](../../packages/compiler/test/CharacterScalar.test.ts), [`StringOwnership.test.ts`](../../packages/compiler/test/StringOwnership.test.ts) |
| 12 | [Operator and short-circuit semantics](../../openspec/changes/archive/2026-08-20-align-operator-and-short-circuit-semantics/proposal.md) | 7 requirements, 17 scenarios, 11/11 tasks | `74b5ffe`, `838b42b` | [`OperatorContracts.test.ts`](../../packages/compiler/test/OperatorContracts.test.ts), [`ShortCircuitOperatorAcceptance.test.ts`](../../packages/compiler/test/ShortCircuitOperatorAcceptance.test.ts) |
| 13 | [Shared pattern destructuring](../../openspec/changes/archive/2026-08-20-add-shared-pattern-destructuring/proposal.md) | 19 requirements, 32 scenarios, 12/12 tasks | `1c36236`, `31449da`, `23556ea`, `0d0cfbe` | [`ExhaustiveMatching.test.ts`](../../packages/compiler/test/ExhaustiveMatching.test.ts), [`EditorIntelligence.test.ts`](../../packages/compiler/test/EditorIntelligence.test.ts) |
| 14 | [Explicit modules catalogs and imports](../../openspec/changes/archive/2026-08-20-make-modules-catalogs-and-imports-explicit/proposal.md) | 3 requirements, 4 scenarios, 13/13 tasks | `203ca35`, `e841411`, `945a9dc` | [`StdlibResolution.test.ts`](../../packages/compiler/test/StdlibResolution.test.ts), [`AutoImport.test.ts`](../../packages/compiler/test/AutoImport.test.ts) |
| 15 | [Source unsafe callable contracts](../../openspec/changes/archive/2026-08-20-add-source-unsafe-callable-contracts/proposal.md) | 2 requirements, 4 scenarios, 12/12 tasks | `b953c7e`, `9056506`, `646713e`, `122e8cc` | [`Elaboration.test.ts`](../../packages/compiler/test/Elaboration.test.ts), [`Document.test.ts`](../../packages/lsp/test/Document.test.ts) |
| 16 | [Matched-toolchain integrity](../../openspec/changes/archive/2026-08-20-verify-matched-toolchain-integrity/proposal.md) | 2 requirements, 3 scenarios, 12/12 tasks | `9878ad1`, `4228c65`, `09b193d` | [`Driver.test.ts`](../../packages/compiler/test/Driver.test.ts), [`IntrinsicAvailability.test.ts`](../../packages/compiler/test/IntrinsicAvailability.test.ts) |

## Lifecycle and gate evidence

Every item has a dated archive and no remaining active roadmap change. Canonical OpenSpec
validation succeeds for every capability touched by the roadmap. The repository-wide gate covers
type checking, Biome, generated-artifact freshness, the evaluator and Wasm suite, native
differential acceptance, CLI, LSP, docs examples, and build output. The release-candidate gate also
validates published package contents and exports.

The separate active legacy OpenSpec lane is not part of this roadmap. Its known
`add-silk-documentation-consumers` delta-format error does not alter any stabilized-core capability
or invalidate the 16 archived changes.
