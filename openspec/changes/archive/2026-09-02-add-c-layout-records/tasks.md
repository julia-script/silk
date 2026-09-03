## 1. Syntax and semantic contract

- [x] 1.1 Parse and recover `[pub] extern "C" struct` on the existing struct node, then verify the focused parser cases pass.
- [x] 1.2 Format the ABI-bearing struct header canonically and verify formatter idempotence in the existing formatter suite.
- [x] 1.3 Add the total struct layout-contract fact, update every source and synthesized fact constructor, and verify declaration fact tests preserve ordinary `Silk` layout.
- [x] 1.4 Preserve the layout contract through presentation and module semantic-surface encoding, and verify round-trip/equality/invalidation tests distinguish it.

## 2. C-layout validation and diagnostics

- [x] 2.1 Implement the recursive C-layout field-admission actor for scalars, raw pointers, non-zero fixed arrays, and nested C-layout records, and verify its focused unit table.
- [x] 2.2 Validate ABI, nongeneric declarations, and resolved fields during declaration completion while preserving invalid nominals, and verify valid and rejected declaration cases.
- [x] 2.3 Add stable structured diagnostics for generic and unsupported-field contracts, regenerate/update the diagnostics catalog, and verify codes and spans.

## 3. Layout and native interoperability

- [x] 3.1 Reuse the nominal target-layout catalog for C-layout records, add verification of the admitted representation invariant, and verify mixed/nested/array offsets, padding, size, and alignment.
- [x] 3.2 Add one host C oracle for `sizeof`, `_Alignof`, `offsetof`, and sentinel field writes, then verify compiler facts and native observations match the host compiler without duplicating analysis.
- [x] 3.3 Add one DriverNativeAcceptance corpus program that calls `clock_gettime` through a mutable C-layout record pointer and verify the native corpus path passes.

## 4. Documentation and OpenSpec

- [x] 4.1 Update the language reference for syntax, allowed fields, ordinary value semantics, opaque-pointer boundaries, by-value exclusion, and unsafe obligations, then verify documentation checks.
- [x] 4.2 Validate the complete OpenSpec change strictly, sync every delta to its main capability, archive the change, and verify no delta remains unsynced.

## 5. Repository verification and review

- [x] 5.1 Run focused affected tests and stabilize the implementation.
- [x] 5.2 Run `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` in required order and record exact results.
- [x] 5.3 Self-review the issue-scoped diff and obtain an independent test-economics approval with base/branch timing evidence for the final committed diff.
