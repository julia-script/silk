# JUL-188 completion progress

This is a chronological investigation log. For current outcomes and remaining gates, see
[implementation-handoff.md](implementation-handoff.md). Earlier pending/failure statements record
the state of those earlier revisions.

The user authorized taking over JUL-188 and merging the scoped callback prerequisite into its branch. PR #427 was merged through commit `234cba3870ca39a88cfbc0bc6fa6422d3cce73af` on PR #424. The verified JUL-187 implementation from main is an ancestor. The original worktree and both stashes remain intact.

## Corrections after the merge

- Named generic callbacks retain their explicit invocation lifetime while inferring their value and service-row arguments. Borrowed-input validity is available during nested Effect comparison, and nominal validation distinguishes bound invocation lifetimes from free captures.
- Aggregate outlives assumptions imply validity of their stored generic parameters. Callable schema binders scope their lifetime metadata as well as parameters/results.
- Fully selected named callbacks prove concrete provider/row constraints before their values lose generic schema metadata. Missing provider implementations remain errors; unresolved or assumed evidence remains subject to the existing escape restriction.
- Mapped provider operations respect the associated provider owner when several providers define the same method name.
- TLS provider selection covers the complete operation row, excludes ambient byte duplex at every access level, and uses explicit reborrows. Named fixture/doc callbacks state one invocation lifetime. Fixture startup handles trust/allocation failures.
- TLS Wasm acceptance moves out of the general Driver file into its own process/CI lane. TLS connection compilation exhausted the default 4 GiB heap after semantic checking; the targeted lane uses the existing TLS-client 6 GiB allowance. CLI/LSP CI jobs run all their tests one file at a time after observed 60 s timeouts under concurrent files.

## Verification so far

Focused type tests: 105 passed. Related callback/provider/inference tests: 65 passed. The scoped-bracket capture/escape regression, including a borrowed generic aggregate, passes analysis and MIR verification. The TLS namespace witness accepts its callback and rejects an ambient-byte-duplex callback. Standard-library documentation generation checked 123 modules with no policy violations. Compiler source builds pass.

TLS native execution, TLS Wasm execution, remaining local checks, and exact-head full CI are still pending. Workspace `pnpm typecheck`, `pnpm format:check`, and `pnpm lint` passed before the latest release-lowering correction. Instance discovery now completes within a 4 GiB heap after exact context-atom interning; 56 focused instance-discovery tests pass. The full TLS fixture then exposes an unavailable exact target in a nonparking obligation, which is still under investigation.

A separate two-second scoped-provider regression exposed a missing release MIR runner: application substitution reintroduced an enclosing generic parameter, and a borrowed provider Effect remained deferred when `catchAll` needed a concrete value. The fixes are published in `dfb5c178`; 48 related focused tests pass, and the regression also passes through an ordinary-source service wrapper. The TLS namespace positive/negative witness passes in 32.38 s locally; its two TLS analyses have an explicit 180 s CI timeout after the previous 60 s shared-runner timeout. The module example now handles initial transport allocation failure, and documentation generation again checks 123 modules with no policy violations.

CI at `f1416909` passed validation, all CLI/LSP tests, three compiler shards, native smoke, TLS-client Wasm, macOS, platform supply, and browser checks. The fourth compiler shard timed out in the namespace test; TLS-connection native/Wasm still rejected compilation. New CI is running on the release-lowering fix. Full exact-head verification and runtime acceptance remain pending; the draft is not ready for final handoff.

The unavailable nonparking target was reproduced with two inherent provider methods sharing a name. Semantic mapping validation respected the owner, but witness construction independently selected the first same-named declaration. Both now use `DeclarationFacts.providerOperation`; the two-second regression accepts the intended provider and verifies its release MIR. Full TLS acceptance is being rerun with this correction.

With provider ownership corrected, the complete TLS fixture passes semantic/nonparking checks and reaches LLVM's MIR verifier (286 s locally), which finds missing provider-specialized acquisition runners. A second two-second regression isolates the path through a scoped callback: invocation completion incorrectly dropped captured outer lifetime arguments, and builtin callback graph edges omitted captured callable identities. Both are now resolved using the concrete callable environment. The standalone module documentation example passes Wasm-target analysis with no diagnostics or resolution failures. The representative Wasm witness covers one authenticated exchange; its factoring leaves the native matrix byte-for-byte unchanged.

The captured-callback correction passes 90 focused suspension/instance/capture tests and workspace typecheck, formatting, and lint. TLS Wasm now reaches MIR verification with its acquisition runners present. Two small regressions isolate the remaining loan failures: a returning match arm kept a loan live at the join and produced duplicate cleanup, while field accesses were compared only by their root local. Lowering now joins only continuing paths, and loan verification preserves field/index selectors (including reborrow ancestry). Endpoint multiplicity remains checked by the existing control-flow proof; the structural verifier no longer counts alternative branch endings as duplicate execution. Focused loan tests and TLS runtime reruns are in progress.

At `3ab416df`, all 69 related loan/slice/selective-catch tests pass, including both minimal regressions and a corrupted-MIR whole-owner-access negative control. Workspace typecheck (18 tasks, 16.011 s), format check (3,794 files, 4.623 s), and lint pass. The representative TLS Wasm module compiles and executes for the first time (254.606 s), returning the fixture's generic typed-error sentinel instead of 42. Inspection finds that `Client.ackWritten` can publish the one-shot authentication event, which `drainOutput` discards; the handshake loop then waits for it again. The adapter now observes already-published authentication metadata after its deadline check. The namespace positive/negative test passes with this correction (35.12 s). Runtime reruns remain pending; this is not final handoff evidence.

Expanding fixture error reporting exposed a separate LLVM match-binding defect before runtime: cleanup of a whole anonymous-union value selected only one member's payload, dropping the outer tag. A two-second structural reproduction uses only a nominal union containing a Vector and an empty alternative; no TLS execution is involved. Target-aware binding selection retains the complete original carrier for whole-value cleanup and retains field selection for actual field bindings. The TLS error reporters use exact startup/connection rows; runtime and affected layout tests are being rerun with this fix.

At `c3591b20`, workspace typecheck (18 tasks, 22.659 s), formatting, and lint pass. All 58 focused layout/match tests pass across the initial run and the host-C rerun with the installed LLVM directory on PATH. TLS Wasm compiles and executes (231.116 s), reporting `ByteIoError.Closed`. The existing native finalized-destroy fixture was strengthened to assert the resource is still live when use begins; it passes success/failure/cancellation (12.22 s). A scoped borrowed-provider case added to the existing byte-duplex corpus reproduces the TLS failure without TLS. Its MIR correctly reborrows a reference field, but LLVM passed the address of that reference slot as the provider address. Loading the selected reference fixes the general field/element reborrow. The scoped byte-duplex native case passes with a fresh compiler cache (18.90 s). One apparent failed rerun reused old emission because the compiler integrity file had not yet been regenerated; the identity is now regenerated before further native checks. Full TLS native/Wasm reruns and exact-head CI remain outstanding.

With selected-reference reborrowing corrected, TLS Wasm reaches its data assertions (216.593 s) instead of a typed transport failure. The fixture returned overlapping code 14; inspection confirms that its exact ClientHello comparison uses a captured `0x0301` record header while Silk emits `0x0303`, already asserted by JUL-187's `matchesHello` and `coreHelloMatches` witnesses. The expected prefix now normalizes only that header byte and still compares every byte; callback result codes occupy a separate range. The OpenSpec delivery artifacts now record verified JUL-187 ancestry and the user's no-new-review-agent-cycle instruction, without claiming independent review approval. Strict OpenSpec validation passes.

Implementation-head PR CI passed the TLS Wasm connection/client lanes, native smoke, platform checks,
validation, package suites, and three compiler shards. The fourth shard passed 566 tests and timed
out its six-program invalid-generic driver group; the unchanged group passed locally in 17.44 s.
The cases now report individually with unchanged assertions and compilation count. Full verification
found that Turbo's strict environment filtered out the requested worker/heap settings, leading to
an LSP timeout; both execution settings now pass through. Eleven existing scheduling/configuration
checks pass. The full native TLS matrix exceeded 4 GiB and is rerunning with the CI TLS allowance
of 6 GiB. Exact-head CI is being restarted after these corrections.

The full native TLS matrix reaches MIR verification in 6 GiB (607.46 s locally; 1019.17 s in CI)
but rejects `expectTruncation` and its recovered Effect because a reused provider runner retains
another invocation's lexical lifetime arguments. A two-second source with two scoped callbacks,
a borrowed generic connection, and direct/recovered method calls reproduces both violations.
Runner reuse intentionally erases lifetime-only arguments; verification now uses the existing
`runtimeArgumentsEqual` for the shared wrapper binding while preserving exact stored-contract and
selected-base checks. The same regression corrupts the provider type and still requires rejection.
It passes in 2.03 s; native acceptance and affected checks are being repeated.

After the wrapper comparison fix, full TLS native acceptance passes MIR verification and reaches
LLVM emission (611.85 s), where duplicate method builders attempt to commit the same function body.
The existing two-second reproduction also reproduces this backend failure. Discovery must retain
separate contextual proofs, but those contexts have identical emitted arguments and concrete
contracts. Lowering now chooses one machine-body instance per emitted specialization/contract,
leaving semantic discovery and its proof checks intact. The small regression asserts unique emitted
symbols and preserves its corrupted-provider negative control; it passes in 2.06 s. Native acceptance
and affected lowering/backend tests are being rerun.

### Native parking callback control

The native matrix then reached LLVM and exposed missing transfer control in the provided
`ParkingDuplex.read` runner (631.01 s locally; confirmed by PR CI). A small scoped-use callback with
captured callback identity and alternate synchronous/parking Clock providers reproduced the error.
Directly run `EffectUseReleaseNonParking` has no retained Effect object for capture lookup.
Provisional lowering now resolves the callable argument and its complete captured target arguments,
and follows both callback providers even when the open callback is initially synchronous. The small
reproduction passed LLVM generation (5.55 s); its retained regression uses cheaper structural MIR
assertions for the parking runner and synchronous control. Full native and exact-head CI remain
pending while those runs complete.

The next native run passed the callback edge and failed at an outer `Effect.result` relay (654.11 s).
Moving the Clock provider outside a small recovered helper reproduced the missing propagation in
4.88 s. The provided-runner worklist now follows recovery's protected Effect and handler even when
initially synchronous, including retained catch bodies. LLVM generation then passed in 5.76 s.
The permanent regression checks both the nested service runner and outer recovered runner under
parking versus synchronous providers. These changes extend the same finite classification worklist;
no provider-wide parking approximation or backend TLS recognition is used.

### Invocation-stable coroutine payload

After provider propagation was repaired, the native matrix reached frame emission and rejected a
borrowed root that changed offset between resume states (648.30 s). A two-suspension source with
changing live temporaries reproduced the same LLVM failure without TLS (5.41 s). The first attempt
used a mutation inside a borrowed capture and was rejected by ownership analysis; the read-only
reproduction isolates layout without that separate source issue.

Frame planning now assigns every retained local one deterministic invocation-wide location. Each
state still contains only its live slots, initialization flags, and cleanup actions. MIR verification
checks these shared offsets instead of requiring independently packed states. This may retain holes
for locals absent from a state; the frame remains bounded by the invocation's retained locals.
The reproduction passed LLVM generation and all five frame tests passed (19.75 s test time).
Its permanent regression checks repeated borrowed-root offsets structurally, without adding a native
process. The full native TLS matrix and CI remain pending on this correction.
