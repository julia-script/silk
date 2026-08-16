## Prerequisite ledger

The pressure program starts from `50ac200181c40527857771f0537a038367289258`, after every
enabling change was merged, synchronized, and archived.

| Issue | Immutable merge | Diagnostic boundary |
| --- | --- | --- |
| #187 representation parameters | `df97fe2bd7d2257aa26666cb0c2c44c319756401` | Added deterministic kind, inference, join, and bound diagnostics (`SEM0104`–`SEM0106`). It deliberately retired no runtime fence: unresolved representations still stop before layout/MIR, and `SEM0103`/`SEM0107` remained. |
| #188 opaque representation results | `0b999a959be48d2bac86b80326a311c106061cd1` | Exact/opaque identity diagnostics (`SEM0108`–`SEM0118`) remain for unresolved, ambiguous, open, private, divergent, cyclic, invalid-binder, missing, and bodyless identities. Valid concrete opaque families now realize without erasure. |
| #189 stored callable fields | `20a2c098ebae815cbcf4ccf206d6c4c8fd71423b` | Narrowed `SEM0103` only for complete concrete nominal callable fields proved by evaluator, LLVM, and Wasm. Unknown/unsupported callable storage remains fenced; `OWN0013` and `OWN0014` retain whole-owner cleanup and receiver-access safety. |
| #190 stored Effect fields | `50ac200181c40527857771f0537a038367289258` | Narrowed `SEM0107` only for complete concrete nominal Effect fields proved by all engines. Unknown/unsupported Effect storage remains fenced; `OWN0015` retains required receiver access. |
| #191 conditional generic conformances | `f472606ca407a20495f9da2684e523e4d852a85e` | Added and retained overlap, decreasing-proof, and failed-proof diagnostics (`SEM0119`–`SEM0121`); only finite, coherent concrete witnesses are admitted. |
| #192 complete interface contracts | `47504fa471794db6378ddc14f92a744b79612000` | Valid complete bound-operation witnesses now lower with exact ownership/failure/requirement facts. `SEM0101` remains for a selected witness with no lowering, while incompatible access and unresolved/conflicting binders remain diagnostic. |

## Bounded spike evidence

- Callable-field realization and cleanup: `CallableFieldRealization.test.ts`,
  `StoredCallableOwnership.test.ts`, `StoredCallableCleanup.test.ts`, and the archived #189
  evaluator/LLVM/Wasm runtime matrix.
- Stored-Effect realization, ownership, cleanup, invalidation, and all-engine parity:
  `StoredEffectLayout.test.ts`, `StoredEffectMir.test.ts`, `StoredEffectOwnership.test.ts`,
  `StoredEffectCleanupVerification.test.ts`, `StoredEffectRuntime.test.ts`, and
  `StoredEffectEngineParity.test.ts`.
- Opaque realization and invalidation: `OpaqueRealization.test.ts`,
  `OpaqueRepresentationEngines.test.ts`, and the opaque cases in `SemanticInvalidation.test.ts`.
- Complete witnesses and conditional proof search: `BoundOperationWitness.test.ts`,
  `CompleteInterfaceContractsFixtures.test.ts`, and `ConditionalConformanceFixtures.test.ts`.
- Static-tree bounded spike: `StaticCompositionCharacterization.test.ts` generates left-associated
  and balanced trees at 1, 8, 32, 64, and 128 leaves in two fresh processes. The checked-in
  characterization report records its deterministic structural gates and empirical backend costs.

The integrated fixture exposed one genuine cross-feature bug: synchronous complete-interface
witness wrappers lacked inherited `effectRunner` classification. The repair was made at the shared
provisional-MIR classification seam rather than weakening a fence, adding a dictionary, or teaching
the compiler any fixture actor name. No failed spike was worked around locally.
