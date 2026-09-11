## 1. Transcript and public ownership

- [ ] 1.1 Add non-consuming SHA-256/SHA-384 checkpoints and verify prefix equality, continued updates, and ownership behavior at the cheapest structured/native tiers.
- [ ] 1.2 Add documented TLS client configuration, limits, progress, errors, authenticated metadata, and affine owner shapes; verify exact signatures, privacy, provider rows, and invalid configuration atomicity with one shared analysis snapshot.

## 2. Handshake and authentication

- [ ] 2.1 Implement bounded ClientHello, ServerHello, extension/order parsing, compatibility CCS, and one exact HRR transcript replacement; verify all suites, groups, retry rules, fragmentation boundaries, and exact transcript checkpoints.
- [ ] 2.2 Implement certificate-list decoding, path and SAN identity validation over one owned leaf graph, CertificateVerify, Finished, key schedule, empty initial client-certificate decline, and delayed authenticated publication; verify precise nested errors and no premature plaintext.
- [ ] 2.3 Implement exact partial feed/output/ack driving, coalesced application data after server Finished, authenticated metadata getters, and bounded application reads/writes; verify stable suffixes, demand precedence, one-shot events, and buffer rules.

## 3. Established traffic and closure

- [ ] 3.1 Implement bounded NewSessionTicket discard and KeyUpdate scheduling/epoch transitions; verify request coalescing, crossing updates, stable old-key output, exact caps, and no retained PSK.
- [ ] 3.2 Implement directional close, close_notify trailing-byte freeze, truncation drain, fatal alerts, sticky failures, and logical state invalidation; verify exact two-byte alerts and closure precedence.

## 4. Evidence and integration

- [ ] 4.1 Add immutable pinned-rustls fixture files, lockfile and generator metadata with exact checksums, test-only key markings, RFC 8448 component evidence, and an offline integrity check.
- [ ] 4.2 Consolidate contract evidence into one shared native corpus matrix, one representative LLVM-to-Wasm witness, and cheap structural assertions; measure focused base/branch runtime and add the native case to scoped PR CI.
- [ ] 4.3 Register the module, generate source and public reference documentation, extract/check/format/run every new example, and verify generated inventories and links.

## 5. Delivery

- [ ] 5.1 Strict-validate OpenSpec, run focused checks in required order, then typecheck, format-check, and lint; commit and push the exact issue-only implementation without local full-suite runs.
- [ ] 5.2 Provide exact diff, fixture provenance, acceptance mapping, local timings, test-economics evidence, and integration notes to the coordinator for independent correctness and distinct TEST_REVIEW review.
- [ ] 5.3 Leave final full test, `pnpm check`, release-candidate, exact-head CI, stacked draft PR, and Linear handoff confirmation to the coordinator; mark complete only after that evidence is read back.
