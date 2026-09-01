# Implementation report

## Result

All 13 implementation tasks are complete. The implementation adds deterministic suspension facts,
execution propagation boundaries, executable property validation, and affine execution lifecycle
facts without adding the construction, drive, wake, park, implicit-entry, or concurrency actors
that the design names as non-goals.

## Gate history

### Pre-conformance hard gates

1. Attempt 1: focused tests passed (70/70), typecheck passed (24/24 tasks), Biome passed,
   and `pnpm test` failed one stale synchronous-cost fixture (2,083/2,084 compiler tests). The
   generated cost expectation was updated for the intentional analysis work.
2. Attempt 2: focused tests passed (71/71), typecheck passed (24/24 tasks), Biome passed,
   `pnpm test` passed (28/28 tasks, including 2,084/2,084 compiler tests), and `pnpm check`
   passed (42/42 tasks plus 16/16 script tests). `pnpm release:candidate` failed because its
   expected compiler export list did not yet include the four new public actors.
3. Attempt 3: focused tests passed (71/71), typecheck passed (24/24 tasks), Biome passed,
   `pnpm test` passed (28/28 tasks), `pnpm check` passed (42/42 tasks plus 16/16 script tests),
   and `pnpm release:candidate` passed (9/9 tests).

### Post-conformance hard gates

- Focused semantic tests: 6 files, 77/77 tests passed.
- `pnpm typecheck`: 24/24 tasks passed.
- `pnpm exec biome check .`: 980 files checked, no fixes required.
- `pnpm test`: 28/28 tasks passed; the compiler suite passed 216 files and 2,090 tests plus
  the one-case native differential acceptance suite.
- `pnpm check`: 42/42 tasks passed plus 16/16 script tests.
- `pnpm release:candidate`: 9/9 tests passed.

## Conformance ledger

The required three fresh lenses were run once. Verified findings were addressed in one consolidated
fix pass. Claims requesting runtime actors or operations were rejected because the change design
explicitly excludes construction, drive, wake, park, implicit entry ownership, and canonical
concurrency actors from this semantic-facts slice.

| Lens         | Claim                                                                  | Severity | Disposition and evidence                                                                                                                                                                                  |
| ------------ | ---------------------------------------------------------------------- | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Language     | External-park ownership and entry admission are scaffolding            | High     | Rejected as out of scope: runtime park and implicit entry ownership are explicit design non-goals; normalized graph facts and diagnostics are present for the downstream slice.                           |
| Language     | Lifecycle and loan rules are descriptive facts                         | High     | Rejected as out of scope for operations: the slice establishes target-neutral lifecycle facts, while existing affine ownership rejects use after move and the new tests cover stable-loan classification. |
| Language     | Production analysis never reports `Open` or `Unavailable`              | High     | Rejected: concrete realized instances are the complete frontier; the public normalization actor represents and tests incomplete graphs for upstream and downstream consumers.                             |
| Language     | Selected-provider propagation is overbroad                             | High     | Verified and fixed: unresolved service calls now resolve only the selected witness operation, and an unused suspending operation no longer contaminates the selected specialization.                      |
| Language     | `Detached` misses string and `Slot` loans                              | High     | Verified and fixed: string backing loans and `Slot` element loans are included in executable environment dependency analysis.                                                                             |
| Language     | An ordinary interface conjunct is reinterpreted as a static property   | High     | Verified and fixed: non-property conjuncts now retain the representation head and emit `SEM0141`.                                                                                                         |
| Language     | Cache identity omits executable properties                             | Medium   | Verified and fixed: parameter construction, serialization validation, and `Type.key` all use the sealed canonical property order.                                                                         |
| OpenSpec     | Suspension behavior is only partial scaffolding                        | High     | Rejected as out of scope: runtime suspension operations are explicit non-goals; direct, nested, external, open, and unavailable facts are normalized and consumed by the semantic pipeline.               |
| OpenSpec     | Delimiting and unowned-root helpers are unused                         | High     | Rejected as out of scope: the change establishes the boundary and diagnostic facts required before the later park/entry slice introduces operation sites.                                                 |
| OpenSpec     | Lifecycle ownership is represented by constants                        | High     | Rejected as out of scope for runtime operations: affine ownership enforcement already exists and the new actor specifies exact state ownership/restoration facts for later lowering.                      |
| OpenSpec     | Provider provenance works only for the special wrapper                 | High     | Verified and fixed: ordinary provide wrappers now preserve borrowed provider provenance as a `ProviderLoan`.                                                                                              |
| OpenSpec     | Property obligations are not checked at every application              | High     | Verified and fixed: represented nominal applications are scanned and validated at every realized application site.                                                                                        |
| OpenSpec     | Property serialization is not canonical                                | Medium   | Verified and fixed with ordered construction, cache identity, and malformed-surface rejection tests.                                                                                                      |
| OpenSpec     | The opaque-result test is not actually opaque                          | Medium   | Verified and fixed: the producer now returns `some<F: Effect<HiddenResult>> F`.                                                                                                                           |
| OpenSpec     | Gate evidence is not recorded                                          | Medium   | Verified and fixed by this implementation report.                                                                                                                                                         |
| Architecture | Delimiting and lifecycle APIs are test-only                            | High     | Rejected as out of scope for runtime consumers: the design deliberately lands semantic prerequisites before the packaging and wake slices.                                                                |
| Architecture | `Open`, `Unavailable`, and external sources are absent from production | High     | Rejected: incomplete states are public normalization inputs, while realized production instances are intentionally concrete; external sources flow through specialized service selection.                 |
| Architecture | Nested represented executable detachment is missed                     | High     | Verified and fixed: environment traversal now follows recursively nested represented executable dependencies.                                                                                             |
| Architecture | Cache canonicality is incomplete                                       | Medium   | Verified and fixed with canonical property ordering and strict deserialization checks.                                                                                                                    |
| Architecture | Locale-sensitive comparison is used for canonical order                | Medium   | Verified and fixed: canonical sorting now compares code units.                                                                                                                                            |
| Architecture | New public actors lack package subpath exports                         | Medium   | Verified and fixed: all four actors have explicit package exports and release-candidate coverage.                                                                                                         |

## Final re-audit and archive verification (2026-08-24)

This report closes against the complete five-change SLP-0001 implementation DAG after integrating
`origin/main` at merge commit `31bdfec`. Repeated independent language/specification,
architecture/standards, and packaging/evidence reviews found no remaining significant defect at
source checkpoint `444b0d9`; low-value stylistic observations were not promoted into change work.
The accepted repairs covered canonical cleanup ordering, native POSIX status handling, callable
loan endpoints, emission-site runtime-feature collection, sealed actor-literal inventory, and
callback-first fatal-path evidence.

The implementation and language-documentation checkpoint `9b4a311` passed `pnpm typecheck` (24/24
tasks), `pnpm exec biome check .` (991 files), `pnpm test` (28/28 tasks, including 220 compiler
files / 2,151 tests and the native differential suite), `pnpm check` (42/42 Turbo tasks plus 16/16
script tests), and `pnpm release:candidate` (9/9 validations). All tasks are complete and no
significant audit finding remains open.
