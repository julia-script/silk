## 1. Owned snapshot actors

- [ ] 1.1 Add `SnapshotLimits`, `TrustLoadLimits`, and the complete structured `TrustSourceError` vocabulary; verify default values, variant access, and inclusive zero/exact/over behavior in the consolidated acceptance source.
- [ ] 1.2 Add opaque `TrustSnapshot` construction and borrowed access; verify allocation-free ownership transfer, checked aggregate accounting, empty explicit trust, order, and duplicate/configured-restriction preservation.
- [ ] 1.3 Add independent copy and combine; verify preflighted limits, exact ordering, lifetime independence, and cleanup for every distinguishing allocation-failure boundary.

## 2. Strict PEM transfer and replaceable source

- [ ] 2.1 Add `CertificateBundle.intoCertificates` and PEM snapshot import; verify no reparse/copy, strict empty/whitespace rejection, malformed-later-block rollback, decoded location preservation, and distinct allocator refusal.
- [ ] 2.2 Add the lexical `TrustSource.load` service and `MemoryTrustSource`; verify nested replacement/restoration, copied loads, atomic allocation-free replace, non-mutating failed loads, and prior-owner validity after replacement/provider drop.

## 3. Portable evidence and registration

- [ ] 3.1 Add one shared native corpus program plus one combined structured ownership/requirement test; verify each distinct target-neutral, cleanup, non-forgeability, and service-row claim without duplicate compilation.
- [ ] 3.2 Add one compact LLVM-to-Wasm witness and explicit pull-request native-smoke selection; verify the selected portable behavior compiles/evaluates and exact-head CI invokes the native corpus case.
- [ ] 3.3 Register all public actors in the canonical manifest, regenerate standard-library source/API artifacts, and verify focused manifest/catalog generation checks pass.

## 4. Documentation and delivery

- [ ] 4.1 Document every public module/member, ownership rule, finite limit, explicit authority boundary, and non-equivalence to OS trust policy; generate and inspect the public reference pages and links.
- [ ] 4.2 Strict-validate this OpenSpec change, run focused behavior/ownership/Wasm tests with timings, then run `pnpm typecheck`, `pnpm format:check`, and `pnpm lint`; leave full `pnpm test`, `pnpm check`, and `pnpm release:candidate` to exact-head CI under the user override.
- [ ] 4.3 Obtain independent correctness and distinct TEST_REVIEW approval on the exact committed issue diff, record the timing delta, and confirm exact-head required CI before handoff.
