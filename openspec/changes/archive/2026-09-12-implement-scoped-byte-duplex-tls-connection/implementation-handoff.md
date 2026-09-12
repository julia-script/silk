# JUL-188 implementation handoff

## Outcome and revision

The implementation is integrated in draft PR [#424](https://github.com/julia-script/silk/pull/424),
base `main`, head branch `julia/jul-188-add-scoped-partial-byte-duplex-io-and-the-tls-connection`.
The scoped callback prerequisite [#427](https://github.com/julia-script/silk/pull/427) was merged at
`234cba3870ca39a88cfbc0bc6fa6422d3cce73af`. Verified JUL-187 merge
`6ab7959ad15841f390f326b8bdc1338a5f732624` is an ancestor. The final implementation revision before
these delivery documents is `a843c5d3f7e8f09d2fa9e4245d52f303f451cb8e`.

This adds partial byte-duplex operations, a bounded deterministic memory provider, and scoped TLS
connections. `withClient` acquires trust and wall time once, enforces one absolute handshake
deadline, invokes the callback only after authentication, and closes its transport lease after
success, typed failure, or structured cancellation. Generic nonparking finalization supplies the
cleanup primitive. Fatal traps remain outside the runtime cleanup contract.

## Direct correctness assessment

This assessment covers the committed issue diff against the verified JUL-187 ancestor, including
the integrated compiler prerequisite and its subsequent corrections. It is the implementation
agent's assessment; the user instructed this session not to start another review-agent cycle.
It is not independent approval or a production-security certification.

- Publication: the handshake checks its absolute deadline before accepting retained authentication
  metadata and again before calling user code. Acknowledging output may publish authentication;
  the adapter observes that retained state instead of waiting for the one-shot event twice.
- Transfer accounting: pending ciphertext remains owned by the client until an acknowledged,
  positive bounded count. Provider errors invalidate the connection, and no unreported suffix is
  treated as accepted. EOF passes through TLS end-of-input validation. Local close-notify/shutdown
  is directional; final release is terminal transport close.
- Ownership: the lease is bracketed before trust/client acquisition. Release failures are handled
  only in the release Effect, preserving the protected outcome. Nonparking finalizers retain
  provider captures across suspended execution and run before ordinary destruction.
- Callback isolation: finite intersections preserve the shorter resource/capture validity;
  invocation regions remain rigid within their binder. Concrete provider evidence is checked,
  and ambient ByteDuplex is excluded from callback rows. Negative tests retain escape/invariance
  and missing-provider rejection.
- Generated code: associated-owner provider selection is shared with witness construction;
  acquisition runners retain captured callable/lifetime arguments. Whole-value match cleanup
  preserves union tags. Stored-reference reborrows load the selected referent. Loan joins account
  for returning arms and disjoint selectors while the CFG verifier checks endpoint multiplicity.
  Shared runner bindings compare emitted arguments after lifetime erasure, while each stored Effect
  and selected base retain exact semantic checks; a corrupted provider type remains rejected.
  Lowering retains one emitted body for identical runtime arguments and concrete contracts;
  contextual proof obligations remain separately checked during discovery.
- Bounds and portability: memory transport storage/audit is bounded and allocation is explicit.
  Scripted readiness and deadline equality use the provided clock. TLS policy and cryptographic
  verification remain in the existing ordinary-source client, without a compiler-known TLS actor.

The valid findings discovered during integration were fixed and exercised by focused regressions.
The remaining delivery gate at the time of this draft is native TLS/full CI completion. Functional
fixtures do not establish constant-time behavior, physical secret erasure, or production security.

## Verification

- The representative TLS LLVM-to-Wasm witness passed at the implementation revision: one test,
  232.61 s. It verifies authentication, application read/write/flush, exact emitted bytes,
  directional shutdown, terminal close, and trust/wall acquisition counts.
- The shared byte-duplex native case passed in 18.90 s with a fresh compiler cache. Its added
  borrowed-provider scope verifies use-before-close and exactly one final close.
- The existing finalized-destroy native case passed in 12.22 s; the fixture now also asserts that
  the resource remains live when use begins. Two native slice tests passed in 2.16 s.
- Focused analysis/MIR/layout groups cover callback lifetime/row inference, provider evidence,
  suspension/finalization, returning match arms, disjoint fields, and tagged union cleanup.
  [TEST_REVIEW.md](TEST_REVIEW.md) records counts, measured costs, and limits. Groups overlap.
- TLS namespace analysis accepts the intended callback and rejects ambient transport access
  (35.12 s). The public example passes Wasm-target analysis. Documentation generation checked
  123 modules with no policy violations; strict OpenSpec validation passed.
- Local `pnpm typecheck`, `pnpm format:check`, and `pnpm lint` passed on the implementation revision.
  Broad local `pnpm test`, `pnpm check`, and release-candidate runs were intentionally excluded.
- Native TLS matrix and exact-final-head CI: pending. PR CI covers the package/compiler suites,
  documentation, native/Wasm acceptance, platform supply, and release candidate. The explicit full
  verification workflow also executes literal `pnpm check` and `pnpm release:candidate`.

Final delivery head and CI links belong in the PR/Linear readback so reporting the observed CI
result does not itself change the revision under verification. JUL-188 advances to In Review only
after these gates pass; PR #424 remains draft, per the delivery contract.

## Preserved state and history

The original JUL-188 worktree and both experimental stashes remain unchanged; exact paths and
identities are recorded in the [scoped callback handoff](../2026-09-12-support-scoped-callback-environments/implementation-handoff.md).
[implementation-progress.md](implementation-progress.md) is the chronological investigation log;
its earlier failures are historical and are superseded by this handoff's current verification.
