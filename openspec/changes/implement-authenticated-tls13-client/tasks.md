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
- [x] 4.2 Partition contract evidence into one complete core and four independently bounded handshake-policy, key-update, closure-control, and resource-policy native corpus programs, one representative LLVM-to-Wasm witness, and cheap structural assertions; measure focused base/branch runtime and add the native cases to scoped PR CI.
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
- The superseded 30-scenario native matrix passed in 360.59 seconds with 3,067,117,568 bytes maximum
  RSS. It remains useful regression evidence, but it does not verify later production repairs. The
  replacement uses one 17,705-byte/557-line complete core plus four independently bounded programs
  for key updates, closure and post-handshake controls, handshake policy, and resource policy. Each
  program has one `Client.make` call path, is no larger than the core, and reads immutable fixture
  bytes through the same data-only C translation unit. C exports only exact size/copy operations and
  never decides TLS outcomes. The exact final focused-source sizes and hashes are recorded after the
  source-only freeze below; native execution remains pending except for the core, which passed in
  135.67 seconds (141.45 seconds command wall, 3,075,080,192 bytes maximum RSS). Its preceding
  default-heap Analysis pass took 76.02 seconds (80.89 seconds command wall, 2,378,481,664 bytes
  maximum RSS). A prior representative Wasm source passed compile, zero-import instantiation, and
  `main() == 42` in 306.62 seconds with 4,436,836,352 bytes maximum RSS. The final independently
  composed source is 22,086 bytes/281 lines, has no native fixture bridge or negative matrix, and
  awaits its exact-source rerun.
- The post-core source-only freeze is: key-update 16,898 bytes/509 lines at
  `c38ba8ba840b404e9eafb3a2895f918fd21ccaefee17cc5b6159a7ca0688a9e4`;
  closure-control 17,322 bytes/522 lines at
  `364e524c8292f8ae21becdbc2a5475c71111f6a71c737b8f59b15c15fd72eacd`;
  handshake-policy 17,591 bytes/553 lines at
  `3d335c779d2b38b15ac516babbd767590702790aa2bf20a8c84b45f715424c59`;
  and resource-policy 17,396 bytes/563 lines at
  `1dbd6d333f29a860a077025961269210350fb7ecbb77e248197e326705e59131`.
  Each contains one `Client.make`; the first three contain one allocator and one random provider,
  while resource-policy contains two allocator providers for its rejecting audit and one random
  provider. All remain below the 17,705-byte core. The deterministic data-only C carrier is 284,504
  bytes/195 lines at `2f07c3992f8c95dfc881f28cae06a8536e79a8787f4d7fa3bfb888c4cf2bc003`.
  Analysis and native execution of these four exact sources remain pending.
- Final review expansion adds sole-record ServerHello/HRR epoch guards, exact nested configuration
  taxonomy, terminal-before-buffer precedence, pending-output cancellation, and focused negatives
  for retry, key-share, ALPN, certificate/control/ticket limits, closure, and exact client control
  bytes. A superseded 63-scenario evidence program exceeded the default compiler heap during source
  realization. That result is an evidence-machinery limit, not a TLS runtime failure. The focused
  replacement does not retain or execute that program; delivery and CI checkboxes remain pending
  until exact-head evidence is read back.
- Final demand review requires `feedInput` and `progress` to drain every immediately complete
  buffered handshake message before advertising `NeedInput`, then schedule any KeyUpdate response
  produced by that work. CertificateRequest review also admits empty `status_request` and
  `signed_certificate_timestamp` requests plus bounded valid `oid_filters`, ignores framed
  unrecognized extensions, rejects every duplicate type including unrecognized types, and
  rejects profile-known extensions that are illegal at that location. Focused exact-head evidence
  for these two repairs remains pending.
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
