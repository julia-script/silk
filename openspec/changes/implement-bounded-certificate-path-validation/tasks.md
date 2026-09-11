## 1. Certificate semantic reuse

- [x] 1.1 Add the smallest allocation-free `CertificateProfile` operations needed to inspect supported SAN and NameConstraints semantics, and verify existing profile acceptance still passes.
- [x] 1.2 Add checked certificate-time conversion and constraint comparison evidence, and verify exact endpoints, invalid dates, DNS wildcards, and IP masks in the consolidated acceptance case.

## 2. Path construction and validation

- [x] 2.1 Add the documented public `silk/certificate_path` API and verify borrowing, getter, scalar-error, and requirement-row behavior with structured compiler assertions.
- [x] 2.2 Implement semantic-free input preflight and inclusive resource counters, and verify exact/one-over limits plus overflow precedence.
- [x] 2.3 Implement bounded iterative anchor-first depth-first search with duplicate/cycle handling and exact original indices, and verify direct, alternate, cross-signed, wrong-signature-first, and cyclic paths.
- [x] 2.4 Implement signature, time, path-length, and cumulative DNS/IP constraint validation, and verify fixed profile acceptance and stable error precedence.
- [x] 2.5 Verify allocation refusal releases partial search/result state without mutating inputs.

## 3. Evidence and documentation

- [x] 3.1 Add the pinned offline fixture manifest and deterministic generation recipe with limbo commit, hashes, validation times, ordering, and deliberate divergence notes; verify its integrity without network access.
- [x] 3.2 Consolidate runtime evidence into one shared native corpus case, one compact Wasm witness, and cheap structural assertions; measure focused runtime and add the native case to PR CI smoke selection.
- [x] 3.3 Register the module, generate API/reference documentation, and verify source generation, doc generation, inventory, links, and examples.

## 4. Verification and review

- [x] 4.1 Strict-validate this OpenSpec change and run focused tests, typecheck, format check, and lint after committing and pushing the intended implementation.
- [x] 4.2 Obtain independent correctness and distinct TEST_REVIEW approval for the exact issue diff; leave this task incomplete until the coordinator confirms both verdicts.
- [x] 4.3 Preserve upstream signature-parameter and empty-subject reasons in path errors; verify exact scalar evidence and alternate-candidate continuation in the consolidated acceptance source, then regenerate and rerun focused gates.
