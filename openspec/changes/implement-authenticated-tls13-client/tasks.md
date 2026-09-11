## 1. Transcript and public ownership

- [x] 1.1 Add non-consuming SHA-256/SHA-384 checkpoints and verify prefix equality, continued updates, and ownership behavior at the cheapest structured/native tiers.
- [x] 1.2 Add documented TLS client configuration, limits, progress, errors, authenticated metadata, and affine owner shapes; verify exact signatures, privacy, provider rows, and invalid configuration atomicity with one shared analysis snapshot.

## 2. Handshake and authentication

- [x] 2.1 Implement bounded ClientHello, ServerHello, extension/order parsing, compatibility CCS, and one exact HRR transcript replacement; verify all suites, groups, retry rules, fragmentation boundaries, and exact transcript checkpoints.
- [x] 2.2 Implement certificate-list decoding, path and SAN identity validation over one owned leaf graph, CertificateVerify, Finished, key schedule, empty initial client-certificate decline, and delayed authenticated publication; verify precise nested errors and no premature plaintext.
- [x] 2.3 Implement exact partial feed/output/ack driving, coalesced application data after server Finished, authenticated metadata getters, and bounded application reads/writes; verify stable suffixes, demand precedence, one-shot events, and buffer rules.

## 3. Established traffic and closure

- [x] 3.1 Implement bounded NewSessionTicket discard and KeyUpdate scheduling/epoch transitions; verify request coalescing, crossing updates, stable old-key output, exact caps, and no retained PSK.
- [x] 3.2 Implement directional close, close_notify trailing-byte freeze, truncation drain, fatal alerts, sticky failures, and logical state invalidation; verify exact two-byte alerts and closure precedence.

## 4. Evidence and integration

- [x] 4.1 Add immutable pinned-rustls fixture files, lockfile and generator metadata with exact checksums, test-only key markings, RFC 8448 component evidence, and an offline integrity check.
- [x] 4.2 Consolidate contract evidence into one shared native corpus matrix, one representative LLVM-to-Wasm witness, and cheap structural assertions; measure focused base/branch runtime and add the native case to scoped PR CI.
- [x] 4.3 Register the module, generate source and public reference documentation, extract/check/format/run every new example, and verify generated inventories and links.

## 5. Delivery

- [ ] 5.1 Strict-validate OpenSpec, run focused checks in required order, then typecheck, format-check, and lint; commit and push the exact issue-only implementation without local full-suite runs.
- [ ] 5.2 Provide exact diff, fixture provenance, acceptance mapping, local timings, test-economics evidence, and integration notes to the coordinator for independent correctness and distinct TEST_REVIEW review.
- [ ] 5.3 Leave final full test, `pnpm check`, release-candidate, exact-head CI, stacked draft PR, and Linear handoff confirmation to the coordinator; mark complete only after that evidence is read back.

## Verification notes

- The optimized native witness initially rejected `Client.readPlaintext` with `InvalidLoan`: the
  application-buffer loan began before the offset expression read `self.applicationRead`. The
  implementation now snapshots that offset before borrowing the buffer and copies inline, matching
  the established record-consumption ownership shape. The same speed-profile rustls replay then
  passed with authenticated coalesced application data and closure assertions intact.
- The mandatory economics revision keeps one corpus registration and one `Client.make` path. A
  30-scenario runtime loop reads capture and mutation bytes from one data-only C translation unit;
  C exports only exact size/copy operations and never decides TLS outcomes. The final optimized
  native run passed in 360.59 seconds with 3,067,117,568 bytes maximum RSS. The representative
  Wasm source is independently composed (21,895 bytes, 266 lines), has no native fixture bridge or
  negative matrix, and passed compile, zero-import instantiation, and `main() == 42` in 306.62
  seconds with 4,436,836,352 bytes maximum RSS.
- The complete rustls matrix exposed and fixed two HRR integration defects: retry ClientHello
  construction no longer resets the outer dispatcher's buffered handshake length, and a valid
  compatibility CCS is accepted while retry output is pending. Malformed CCS syntax remains owned
  by the JUL-171 record layer (`ProtocolReason.Record`); a separate valid CCS in an illegal client
  state proves the client-owned `ProtocolReason.CompatibilityCcs` taxonomy and stickiness.
- Identity-negative replay uses exact pinned rustls transcripts for `wrong.example` SNI and an IPv4
  reference with no SNI. Cheap host guards parse those ClientHellos, decrypt their certificate
  messages, and require the canonical RSA leaf plus root byte-for-byte, so code 53 proves that path,
  record, transcript, and CertificateVerify checks succeeded before identity alone rejected.
- Generated documentation passes the 120-module policy check, and the executable public example
  passes with all 58 standard-library doctests after rebuilding the compiler artifact used by the
  CLI. The generated source identity and rendered reference contain the same lifetime-explicit
  example and no removed diagnostic helper.
