## 1. Decoder

- [x] 1.1 Add the public decoder state, limits, progress and typed errors with bounded construction; verify source analysis and allocation/limit behavior.
- [x] 1.2 Implement resumable stored, fixed and dynamic DEFLATE decoding and circular history; verify independent fixtures and malformed coding cases in the shared native corpus.
- [x] 1.3 Implement zlib/gzip parsing, checksums, final input, concatenated members and cumulative limits; verify byte boundaries, output suspension, trailer failures and terminal reuse in the shared native corpus.

## 2. Integration and evidence

- [x] 2.1 Add published and pinned independent differential fixture provenance and consolidated native corpus coverage; verify focused execution without per-vector compilation.
- [x] 2.2 Register the module, document the public contract, and regenerate stdlib/reference surfaces; verify stdlib and documentation checks and executable example.

## 3. Verification and delivery

- [x] 3.1 Run typecheck, format:check, lint, test, check and release:candidate in repository order; resolve failures and record exact evidence.
- [x] 3.2 Complete independent implementation review and separate mandatory test-economics review with equivalent base/head timings; resolve findings and obtain approval for the final committed diff.
- [x] 3.3 Commit and push the issue-scoped change, confirm a draft PR, and update JUL-163 to In Review with exact PR-head baseline and verification evidence.

Delivery: [draft PR #398](https://github.com/julia-script/silk/pull/398); JUL-163 is In Review. Verification evidence and independent review results are recorded in the PR and Linear handoff.

## 4. Independent-stream reset (JUL-174)

- [x] 4.1 Add allocation-free reset with atomic limit rejection and complete fresh scalar state while preserving owned storage.
- [x] 4.2 Extend consolidated native acceptance with success/failure/abandonment, format/window, stale history, counters/checksums/final input/Huffman, and rejected-reset continuation witnesses.
- [x] 4.3 Update source documentation and prescriptive lifecycle text; regenerate standard-library and documentation surfaces.
- [ ] 4.4 Run required verification and release-candidate checks; obtain independent code and test-economics approval of the committed diff.
- [ ] 4.5 Confirm the draft PR and JUL-174 In Review handoff with the exact reviewed head and verification evidence.
