## 1. Admitted public keys and arithmetic

- [x] 1.1 Implement bounded key admission and public arithmetic; verify 2048/2049/4096-bit known answers and invalid key/representative cases.
- [x] 1.2 Implement bounded RSA key and algorithm DER admission; verify absent/NULL/default/explicit parameter forms, unused bits and malformed encodings.

## 2. Complete verification

- [x] 2.1 Implement SHA-256 PSS and certificate PKCS#1 verification; verify complete padding, leading-bit, emLen and DigestInfo rejection.
- [x] 2.2 Add documented source, manifest and generated references; verify source formatting and documentation policy.

## 3. Evidence and handoff

- [x] 3.1 Pin independent fixture provenance and reproduction; verify selected NIST and Zig-positive results plus standards-derived negatives.
- [x] 3.2 Complete economical shared native, structured and Wasm evidence plus emitted-target inspection; record exact results and limits.
- [ ] 3.3 Run required repository and release checks, resolve independent correctness and test-economics review, and verify the draft PR and Linear handoff.
