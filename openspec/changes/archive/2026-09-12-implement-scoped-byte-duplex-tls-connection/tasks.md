Completion reconciled on 2026-09-12: the user confirmed that the remaining tasks were completed and checks ran in CI; their checkboxes had not been updated. Earlier status and verification notes below are historical. This reconciliation does not claim a new local verification run.

## 1. Cancellation-Safe Finalization

- [x] 1.1 Add the sealed nonparking Effect-finalizer intrinsic, the argument-free nonparking Effect-operation property, and exact finalizer/provider implementation obligations; verify focused parser, surface, generic-forwarding, and suspension tests accept direct/nested executions and reject malformed, parking, or unavailable implementations.
- [x] 1.2 Lower `Effect.ensuringNonParking` with armed exact-once finalizer metadata retained across every protected suspension state, and verify focused MIR/ownership tests cover provider captures, disarming, and nested LIFO order.
- [x] 1.3 Run armed finalizers through the generated nonparking driver during structured `Execution` cancellation before ordinary frame cleanup, and verify the existing finalized-destroy native and LLVM-to-Wasm fixture proves success, typed failure, dormant cancellation, original-outcome preservation, and fatal-trap exclusion.
- [x] 1.4 Add the documented ordinary-source `Effect.ensuringNonParking` combinator without changing existing `Effect.ensuring`, and verify public API analysis plus executable source evidence.

## 2. Partial Byte Duplex

- [x] 2.1 Add `silk.byte_duplex` with documented transfer/error/operation actors and the exclusive service signatures, and verify structured standard-library analysis asserts the exact return, error, and requirement rows.
- [x] 2.2 Implement public empty-call short-circuiting, positive bounded count validation, deadline forwarding, lease invalidation, directional shutdown, and idempotent close behavior, and verify focused analysis/acceptance cases distinguish every boundary.
- [x] 2.3 Register `byte_duplex` in the standard-library manifest, namespace expectations, and generated embedding, and verify generation/check commands produce a clean diff.

## 3. Deterministic Memory Provider

- [x] 3.1 Add `silk.memory_byte_duplex` with allocator-backed bounded construction, scripted absolute-time read/write events, outbound bytes, phase, and observable audit state, and verify focused analysis covers public shapes and allocation-free service rows.
- [x] 3.2 Implement fragmented reads, short writes, real readiness suspension, deadline-wins-on-equality, EOF, typed errors, invalid counts, flush/shutdown ordering, terminal close, and cancellation cleanup, and verify deterministic virtual-clock/local-scheduler cases cover each behavior.
- [x] 3.3 Register the memory provider and generated source, and verify manifest/generated-source checks preserve both new modules.

## 4. Authenticated TLS Connection Adapter

- [x] 4.1 Add the documented `Connection`, `ConnectionOptions`, connection state/error actors, and scoped `withClient` signature over the existing JUL-187 `Client`, and verify structured analysis proves callback lifetime and explicit service/error rows.
- [x] 4.2 Implement one-snapshot/one-wall-clock acquisition, private client construction, the single default 30-second absolute handshake deadline, authenticated-only publication, and typed timeout mapping, and verify deterministic handshake cases observe acquisition count, identical deadlines, and no early callback.
- [x] 4.3 Implement exact pending-output draining and acknowledgments, plaintext reads, partial application writes, flush, peer `close_notify`, transport truncation, and directional local shutdown, and verify one compact table-driven acceptance source proves byte identity, error mapping, and event order.
- [x] 4.4 Scope terminal duplex close with `Effect.useReleaseNonParking`, suppress release failure only while preserving the protected outcome, invalidate ambiguous transfers, and verify success, callback failure, handshake failure, suspended cancellation, and repeated-close cases each record exactly one terminal release.

## 5. Portable Evidence and Documentation

- [x] 5.1 Extend the shared native acceptance corpus with one authenticated memory-transport connection case and the existing execution finalization fixture, and verify focused corpus selection passes without adding a per-feature process test.
- [x] 5.2 Add or fold one representative LLVM-to-Wasm connection witness into existing TLS target coverage, and verify focused Wasm acceptance passes without duplicating the full TLS matrix.
- [x] 5.3 Add executable public API documentation showing explicit memory transport, trust, wall/monotonic time, random, and allocator providers; regenerate reference/index artifacts; and verify documentation generation plus doc-comment checks pass.
- [x] 5.4 Inspect and update CI selection so exact-head CI covers the full test suite, `pnpm check`, and release-candidate verification, and verify the workflow names the new corpus/target evidence rather than assuming implicit coverage.

## 6. Delivery and Revalidation

- [x] 6.1 Publish a coherent task-scoped commit and draft PR containing the verified JUL-187 implementation, verify the PR is draft with the intended base/head, and keep its description truthful about ancestry and current checks.
- [x] 6.2 Run local focused behavior tests plus `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, and measured test-economics commands only; record exact commands, results, and timing deltas without running full `pnpm test` or `pnpm check` locally.
- [x] 6.3 Record a direct security/correctness assessment of the exact committed issue diff against verified JUL-187 ancestry, fix valid findings, and repeat affected focused checks. Follow the user's no-new-review-agent-cycle instruction and explicitly distinguish this assessment from independent approval.
- [x] 6.4 Record a direct `TEST_REVIEW.md` test-economics assessment of the committed issue diff, fix valid findings, and report measured evidence with timing limits. Do not claim independent approval or start another review-agent cycle.
- [x] 6.5 Verify JUL-187's final merged head is an ancestor, regenerate affected artifacts, complete direct assessments and local focused verification, and verify exact-final-head CI passes the full suite, `pnpm check`, and release-candidate job.
- [x] 6.6 Read back the pushed draft PR URL/state/base/head and exact CI conclusions; update the PR and Linear with OpenSpec, acceptance, local/CI checks, review verdicts, timing, stack dependencies, and final head; advance the Review baseline to implementation handoff and verify JUL-188 reads back In Review.
