## 1. Record ownership and protection

- [x] 1.1 Add the `silk.tls_record` public types, exact protected/plaintext constructors, derived suite keys, fixed-capacity direction storage, and private sequence state; verify constructor widths, allocation channels, and manifest resolution with focused analysis.
- [x] 1.2 Implement protected and plaintext queue/pending/acknowledgment driving with exact prefix accounting, stable pending bytes, record fragmentation, atomic misuse errors, TLS headers/AAD/nonces, and no post-construction allocation; verify the focused native record program.
- [x] 1.3 Implement header-first fragmented receive driving, authenticated delayed publication, inner content/padding validation, terminal peer failure, owner-borrowed record inspection, and explicit consumption; verify fragmentation, coalescing, tampering, empty-buffer, and lifetime cases.

## 2. Evidence and integration

- [x] 2.1 Commit RFC 8448 SHA-256 and pinned independent AES-256/SHA-384 and ChaCha fixtures with immutable provenance; verify expected wire bytes through the focused native fixture.
- [x] 2.2 Add the minimum distinct shared native-corpus, structured-analysis, and LLVM-to-Wasm cases, including canonical-source private near-cap construction; measure the focused default-suite cost and verify no per-feature native harness or live network test is added.
- [x] 2.3 Add complete public doc comments, standard-library manifest/catalog registration, generated reference output, and the key-possession-versus-identity warning; verify focused documentation and generated-file checks.

## 3. Delivery

- [x] 3.1 Run strict OpenSpec validation, focused behavior tests, then local `pnpm typecheck`, `pnpm format:check`, and `pnpm lint`; commit and push the exact issue diff for CI full-suite and release-candidate verification.
- [ ] 3.2 Complete independent correctness/security and separate TEST_REVIEW test-economics approval against the committed issue diff, fix findings, and provide exact head/evidence to the coordinator for the stacked draft PR and Linear handoff.
