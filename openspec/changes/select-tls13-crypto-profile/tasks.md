## 1. Selected design delivery

- [x] 1.1 Revalidate the issue against the current work-base delta and record the delivered HMAC/HKDF and current entropy availability in design.md.
- [x] 1.2 Select the complete algorithm and key/parameter matrices and write both delta specs; verify every JUL-166 acceptance topic has a concrete contract.
- [x] 1.3 Map each operation to authoritative known-answer material and an immutable independent implementation in evidence.md; verify the pinned reference paths exist.
- [x] 1.4 Create independently estimated implementation issues with native prerequisites and confirm their states and relations by readback; record them in implementation.md.

## 2. Future primitive implementation (separate issue runs)

These tasks remain unchecked after JUL-166 handoff. The design's delivery does not mean the
future runtime contracts are implemented. Each issue must first pass triage and then create or
refine its own implementation OpenSpec change; this list coordinates acceptance, not a bulk apply.

- [ ] 2.1 Deliver JUL-175 AES/GHASH/GCM and prove both key sizes, bounds and failed-open destination preservation using the pinned evidence map.
- [ ] 2.2 Deliver JUL-176 ChaCha20/Poly1305 and prove component/composition vectors, counter bounds and authenticated output behavior.
- [ ] 2.3 Deliver JUL-177 P-256 arithmetic/agreement and verify curve/scalar rejection, secure generation and RFC/NIST shared secrets.
- [ ] 2.4 Deliver JUL-178 X25519 and verify standardized decoding/clamping, all-zero rejection and exchange vectors with explicit entropy.
- [ ] 2.5 After JUL-177, deliver JUL-179 ECDSA verification and verify DER/range/parameter policy, high-s acceptance and independent signature vectors.
- [ ] 2.6 Deliver JUL-180 RSA verification and prove bounded key policy, strict PKCS1/PSS encodings and negative signature/parameter cases without private RSA APIs.
- [ ] 2.7 Over delivered JUL-162, deliver JUL-181 TLS labels and derivation; verify byte framing, hash/context distinction, both hash outputs and failure-before-write boundaries.

## 3. Future profile admission

- [ ] 3.1 Check each primitive's manifest/reference integration, supported-target evidence, independent security review and measured test-economics approval before advertising it as available.
- [ ] 3.2 Reconcile the delivered primitive set with JUL-168/JUL-171 implementation work and verify all selected algorithms and provider requirements before making any complete TLS/profile support claim.
