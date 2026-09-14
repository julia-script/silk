# Handoff: finish the HTTP tickets already in progress

## Checkout and workflow

- Repository: `julia-script/silk`
- Checkpoint branch: `julia/http1-wip-checkpoint-2026-09-13`
- Base before the checkpoint: `359926dc` on PR #438.
- Read `.codex/skills/silk-work/SKILL.md`, the repository `AGENTS.md`, the Effect patterns skill,
  the OpenSpec apply instructions, and the Silk doc-comment standard before editing.
- Treat this repository as greenfield: delete superseded paths and do not add compatibility shims.
- Prefer the codebase-memory graph for structural discovery.
- Keep compiler-heavy tests serial. Do not run native and Wasm/compiler realization jobs in
  parallel.
- Preserve `.pnpm-store/v11/index.db-shm` and `.pnpm-store/v11/index.db-wal`; never stage them.

The existing visualization stack is:

1. PR #431 `stack-01-foundations` -> `main`
2. PR #432 `stack-02-parsing` -> #431
3. PR #433 `stack-03-resolution` -> #432
4. PR #434 `stack-04-transport-framing` -> #433
5. PR #435 `stack-05-hardening` -> #434
6. PR #436 `stack-06-prerequisites` -> #435
7. PR #437 `stack-07-runner-lowering` -> #436
8. PR #438 `stack-08-known-provider-inference` -> #437

Continue with small stacked PRs. The lower PRs do not need independent green CI because the stack
will merge together; only the current top PR's exact head is authoritative.

## Cross-cutting compiler prerequisite

Finish the Effect execution-contract / conditional-witness runner work first using
`handoffs/2026-09-13-effect-runner-execution-contract.md`. It blocks reliable runtime evidence for
buffered I/O, content decoding, owned TLS, and the server. Do not compensate for it in standard
library code.

## JUL-192 — Buffered byte I/O

Implementation and static evidence are substantially complete:

- `packages/compiler/stdlib/silk/buffered_duplex.silk`
- `packages/compiler/test/BufferedByteIo.test.ts`
- `packages/compiler/test/support/bufferedByteIoAcceptance.ts`
- `apps/docs/content/reference/buffered-byte-io.md`
- `openspec/changes/implement-buffered-byte-io/`

OpenSpec tasks 1.1-3.2 are checked. Task 3.3 remains open for the minimum distinct native/Wasm
runtime evidence and cost decision. Static review fixed a stray u64 literal. A compact portable
fixture was about 168 lines versus 1334 native-source lines (roughly 87% smaller), but final
full-versus-compact backend timing/evidence was not completed because of the runner blocker.

After the compiler prerequisite is green, run focused structured tests first, then the selected
buffered native case and the compact Wasm case. Check 3.3 only when both distinct oracles are real.
Publish this as the next ticket-scoped stacked PR after the compiler layer.

## JUL-196 — Streaming content decoding

Files:

- `packages/compiler/stdlib/silk/http_content.silk`
- `packages/compiler/test/HttpContent.test.ts`
- `packages/compiler/test/support/httpContentAcceptance.ts`
- `apps/docs/content/reference/http-content-decoding.md`
- `openspec/changes/implement-http-content-decoding/`

Public model, response context, `withReader`, negotiation, manifest registration, and docs exist.
The production implementation includes planning, decoder composition/state, progress/error handling,
metadata, and completion paths, but most runtime/task evidence is still unchecked.

Important evidence repair already saved:

- the planning test now realizes one retained `planning` function and runs strict MIR verification;
  it no longer claims behavior by searching source strings;
- planning cases cover lower/upper limits, a real five-coding depth excess, exact `maxOwned`
  boundaries, and fixed/identity/inflate/Zstd reservation accounting;
- the runtime alias case uses mixed-case `X-GZIP`;
- OpenSpec tasks 1.3 and 1.4 were deliberately reopened until that focused realization test passes.

First pass the runner collision regression from the compiler handoff. Then run only the focused
planning realization/MIR test. If green, re-check 1.3/1.4. Next run the selected native case with a
6 GiB heap:

```sh
SILK_NATIVE_CORPUS_CASES=http-content-decoding \
pnpm --filter @silklang/compiler exec vitest run test/DriverNativeAcceptance.test.ts \
  -t "runs the native corpus case" --maxWorkers=1 --max-old-space-size=6144
```

Use the repository's established heap invocation if the option must be passed through `NODE_OPTIONS`.
Then run only the pruned portable Wasm fixture. Keep task boxes open until each stated oracle passes.

## JUL-146 — Native socket connections

Implementation, docs, manifest entries, target witnesses, and acceptance source are present in:

- `packages/compiler/stdlib/silk/native_socket.silk`
- `packages/compiler/test/NativeSocket.test.ts`
- `packages/compiler/test/support/nativeSocketAcceptance.ts`
- `packages/compiler/test/NativeToolchain.test.ts`
- `apps/docs/content/reference/native-socket-connections.md`
- `openspec/changes/implement-native-socket-connections/`

All OpenSpec implementation boxes are currently checked, including owned acquisition, cancellation,
deadline resampling, exact close ordering, ByteDuplex translation, native witnesses, publication,
and docs. Treat this as implementation-complete but not handoff-complete until focused tests and the
selected shared native corpus pass on the final compiler layer. Do not reopen live DNS/public-network
tests; fixtures are deterministic and local.

## JUL-191 — Listeners and accept

Implementation is present in the shared `native_socket.silk` actor plus:

- `packages/compiler/test/NativeListener.test.ts`
- `packages/compiler/test/support/nativeListenerAcceptance.ts`
- `apps/docs/content/reference/native-socket-listeners.md`
- `openspec/changes/implement-native-socket-listeners/`

All OpenSpec boxes are checked. The surface covers owned listeners, scoped accept, accepted contexts,
TCP/Unix acquisition, serial deadline-aware accept, peer/bound metadata, error mapping, Darwin/GNU
witnesses, shared corpus integration, Wasm exclusion, and docs. After the compiler prerequisite,
run focused listener analysis/MIR before the one selected native corpus case. Keep JUL-146 and
JUL-191 in separate stacked PRs even though they share the actor and generated files; stage exact
hunks or use a temporary split worktree.

## Owned TLS prerequisite for JUL-197

This is started but not complete. Files:

- `packages/compiler/stdlib/silk/tls_connection.silk`
- `packages/compiler/test/support/tlsConnectionAcceptance.ts`
- `packages/compiler/test/DriverTlsConnectionWasmAcceptance.test.ts`
- `openspec/changes/add-owned-tls-connections/`
- TLS sections in `apps/docs/content/reference/runtime-and-standard-library.md`

Static implementation work includes owned connection state, zero-length phase handling, cancellation
owner probes, handshake deadline precedence, graceful shutdown drain/flush/write-shutdown, and
cleanup audits. The compact Wasm source was roughly 52% smaller; the namespace slice passed with one
test active and eleven skipped. Most OpenSpec boxes remain open because the complete migration and
runtime evidence were not run.

After the compiler layer, run focused TLS realization/strict MIR, then the selected native case
whose expected sentinel is 42, then the compact Wasm case. Reconcile tasks 1.1/1.2 with the now-shipped
owned native socket APIs, remove obsolete trust-loading paths, finish caller migration, and only then
check the owned TLS tasks. This prerequisite must be complete before starting JUL-197 connection
pooling; pooling itself has no implementation progress in this checkpoint.

## JUL-201 — Streaming HTTP server

Files:

- `packages/compiler/stdlib/silk/http_server.silk`
- `packages/compiler/stdlib/silk/http_server_native.silk`
- `packages/compiler/test/HttpServer.test.ts`
- `packages/compiler/test/support/httpServerAcceptance.ts`
- `packages/compiler/test/support/httpServerPortableAcceptance.ts`
- `apps/docs/content/reference/http-server.md`
- `openspec/changes/implement-streaming-http-server/`

The saved implementation has undergone static repairs for conditional caller/source admission,
BodyError cursor/wire accounting, discard ceilings, output progress, phase guards, trailer rejection,
short-write provider failures, and tunnel normalization. A compact Wasm source was about 291 lines
versus 1885 native-source lines (roughly 85% smaller). The reduced negative conditional-conformance
case passed earlier; the positive server realization is blocked on the compiler runner work. All
server OpenSpec boxes remain open and must stay open until focused structured and runtime evidence
passes.

After the compiler regression is green, run the reduced positive server case, then the full focused
`ConditionalConformance.test.ts`, then `HttpServer.test.ts`. Only after those pass run the selected
portable/native fixtures. Validate the implementation against every task before checking boxes;
current source breadth is not evidence by itself.

## Shared/generated-file coordination

These files mix several tickets and must be split deliberately:

- `packages/compiler/stdlib/manifest.json`
- `packages/compiler/src/Stdlib.generated.ts`
- `packages/compiler/test/StdlibNamespaceAcceptance.test.ts`
- `packages/compiler/test/DriverNativeAcceptance.test.ts`
- `packages/compiler/test/support/corpus.ts`
- `apps/docs/content/reference/index.md`

Regenerate only after the intended ticket source is staged. For stacked PRs, use exact hunks so each
layer contains only its module entries and corpus registrations. Do not stage the whole dirty tree
into a ticket PR.

## Explicitly out of scope for this handoff

No implementation progress was made here for JUL-23, JUL-198, JUL-199, JUL-197 pooling itself,
JUL-202, JUL-203, or JUL-200. Do not include them merely because they are downstream in the original
ticket list. Base64, head parsing, body framing, address resolution/DNS cancellation, and earlier
socket prerequisites are already represented by the committed lower stack and should not be redone.

## Completion and publication order

1. Finish and publish the compiler runner layer as stack PR 09.
2. Finish/publish JUL-192.
3. Finish/publish JUL-146 and JUL-191 as separate coherent layers.
4. Finish/publish JUL-196.
5. Finish/publish the owned TLS prerequisite.
6. Finish/publish JUL-201.
7. Update every PR body with the full chain and mark only the newest top as the authoritative CI
   gate. Wait for required CI on that exact top head; repair only failures attributable to the stack.
