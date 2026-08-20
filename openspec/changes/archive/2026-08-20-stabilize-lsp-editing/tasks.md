## 1. Compiler Recovery Contract

- [x] 1.1 Add compiler regression cases for incomplete array, reference, parenthesized, callable, and row-type match-pattern prefixes, and verify `pnpm exec vitest run packages/compiler/test/Parser.test.ts packages/compiler/test/Elaboration.test.ts` completes without an escaped defect.
- [x] 1.2 Add `ErrorPattern` to the concrete syntax vocabulary, make pattern parsing retain invalid pattern tokens with one parser-owned recovery diagnostic, and verify parser shape/span assertions cover `match`, pattern `let`, and `if let` positions.
- [x] 1.3 Add the semantic `UnavailablePattern` fact and update coverage, occurrence, tooling, HIR, ownership, and lowering consumers exhaustively; verify the targeted exhaustive-matching and analysis-facade tests preserve unrelated facts and emit no speculative semantic diagnostic.
- [x] 1.4 Audit source-reachable elaboration child assertions adjacent to recovered expressions and patterns, replace recovery-reachable throws with the smallest explicit unavailable fact, and verify a deterministic prefix/deletion canary corpus builds frontend snapshots successfully.

## 2. Resilient Project Scheduler

- [x] 2.1 Extend `ProjectSession` tests with controlled analysis success, defect, interruption, and publication failure exits; verify every case settles exact-version waiters, permits a later valid revision to publish, and allows shutdown to finish under `TestClock` without timing assertions.
- [x] 2.2 Refactor project-session construction into a scoped actor whose `open`, `close`, and `invalidate` operations only capture desired state and signal a supervisor; verify document update operations return before the controlled analysis callback is released.
- [x] 2.3 Replace the fixed sleep loop with a capacity-one latest-state signal and resettable trailing-edge debounce, and verify a burst starts only the final captured revision while preserving at most one active analysis.
- [x] 2.4 Represent active analysis with an owned fiber, interrupt it when a newer revision is accepted, and finalize the fiber through `Exit`/`Cause`; verify stale, failed, and interrupted work never commits or becomes the reuse basis.
- [x] 2.5 Make waiter settlement and shutdown derive from actor state and scope closure rather than an `active` boolean/idle deferred pair; verify close, supersession, current failure, active shutdown, and post-defect shutdown all terminate deterministically.

## 3. Cooperative Frontend Preemption

- [x] 3.1 Add Effect interruption/yield checkpoints between project closure, header/index, module semantics, ownership, and tooling phases while preserving immutable results and observation ordering; verify existing project-analysis reuse and determinism tests pass unchanged.
- [x] 3.2 Split remaining global module loops into deterministic bounded batches at the checkpoint boundary, and verify interruption stops obsolete work before every remaining module is processed while a non-interrupted run produces byte-for-byte equivalent observations and diagnostics.
- [x] 3.3 Add an opt-in LSP pressure benchmark that reports accepted edits, analysis starts/completions/interruptions, semantic reuse, phase durations, and final-edit commit latency; verify sustained input retains one latest pending revision and the benchmark contains no pass/fail timing threshold.

## 4. Server Synchronization and Diagnostics

- [x] 4.1 Replace the server-global document-update barrier with ordered synchronization handles keyed by document URI and project-scoped invalidation signaling; verify an unrelated project's controlled update cannot delay a request for an already committed project.
- [x] 4.2 Route semantic requests through exact-version project acquisition and settle requests with no result on supersession, failure, interruption, close, or shutdown; verify `packages/lsp/test/Server.test.ts` covers each terminal outcome without reading an older line index.
- [x] 4.3 Add a project-session failure publication event that logs the internal `Cause` and publishes an empty diagnostic set tagged with the failed current document version; verify old diagnostics clear and the next valid edit publishes its complete diagnostics.
- [x] 4.4 Make server shutdown close every project scope before disposing shared runtime/protocol resources, and verify stdio shutdown completes after a controlled analysis defect and during controlled active analysis.
- [x] 4.5 Add a rapid-edit stdio regression using a controlled slow analysis boundary, and verify only the newest revision publishes while hover/inlay requests and a second project remain live.

## 5. Extension Restart Lifecycle

- [x] 5.1 Extract an Effect-owned language-client lifecycle actor with injectable client construction, serialized restart calls, and explicit absent/running state; verify its unit tests cover activation and normal stop/start replacement.
- [x] 5.2 Replace direct `LanguageClient.restart()` use with retire-and-create-fresh behavior after both successful and timed-out stops; verify tests prove one command starts a replacement and a replacement-start failure leaves no client marked running.
- [x] 5.3 Update the extension README's restart guidance to describe automatic recovery from an unresponsive server and the remaining window-reload cases, and verify the documented command names match `package.json` contributions.

## 6. Verification and Live Reproduction

- [x] 6.1 Run the focused compiler, project-session, server, and extension tests introduced above and verify every regression reproduces its pre-fix failure mode structurally rather than through elapsed-time assertions.
- [x] 6.2 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test`, fixing all failures attributable to this change and recording any pre-existing failure explicitly.
- [x] 6.3 Run `pnpm check` and `pnpm release:candidate`, and verify package contents, generated diagnostics, and explicit exports remain coherent after the new syntax/fact variants.
- [x] 6.4 In the Cursor Extension Development Host, reproduce an incomplete match pattern, repair it to valid source, confirm diagnostics and semantic requests recover without restart, then run `Silk: Restart Language Server` once and verify the replacement server initializes without a stop-timeout error.
