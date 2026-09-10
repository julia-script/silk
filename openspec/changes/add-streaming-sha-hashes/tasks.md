## 1. Canonical Modules and Packaging

- [x] 1.1 Add `silk/sha1`, `silk/sha2`, and `silk/sha3` to the deterministic standard-library manifest and verify the manifest generator recognizes all three identities.
- [x] 1.2 Implement the canonical `Sha1` actor with `make`, inherent mutable `update`, consuming `finish`, and one-shot `hash`; verify empty, `abc`, segmented, and 55/56/57/63/64/65-byte boundary digests against independent expected bytes.
- [x] 1.3 Implement the 32-bit SHA-2 core and `Sha224`/`Sha256` actors with the shared lifecycle and checked 64-bit bit count; verify both variants plus 55/56/57/63/64/65-byte boundary digests.
- [x] 1.4 Implement the 64-bit SHA-2 core and `Sha384`/`Sha512`/`Sha512_224`/`Sha512_256` actors with carry-correct two-word bit counts; verify all variants plus 111/112/113/127/128/129-byte boundary digests and focused carry/overflow behavior.
- [x] 1.5 Implement the Keccak-f[1600] core and four fixed-output SHA-3 actors; verify every variant plus chunking and each distinct rate boundary against independent expected bytes.
- [x] 1.6 Regenerate the embedded standard-library catalog and verify the checked-in generated source and manifest integrity agree with the canonical `.silk` files.

## 2. Behavioral Verification

- [x] 2.1 Add one consolidated shared native acceptance case covering all eleven empty and `abc` known-answer vectors, empty updates, repeated small updates, one-shot/segmented equivalence, SHA-1/SHA-2 padding boundaries, and SHA-3 rate boundaries; verify the focused native acceptance command passes.
- [x] 2.2 Add only non-redundant structured analysis assertions needed to prove inherent method resolution and consuming finalization, and verify those focused compiler tests pass.
- [x] 2.3 Confirm the implementation adds no compiler-known SHA operation, intrinsic, provider, service, Effect requirement, external crypto import, or heap allocation by reviewing the source/diff, running native behavior checks, and running direct-WebAssembly analysis/codegen checks.

## 3. Public Documentation

- [x] 3.1 Document all three modules and every public actor/member with digest sizes, streaming and one-shot guidance, consuming finalization, the distinction from `silk/hash`, and the SHA-1 legacy warning; verify documentation policy and doctests pass.
- [x] 3.2 Regenerate the standard-library reference and verify its index and SHA module pages are current, deterministic, and include a concise compilable streaming example.

## 4. Repository Verification and Handoff

- [x] 4.1 Run focused SHA checks, then `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`; record exact outcomes and distinguish any pre-existing failure.
- [ ] 4.2 Measure the focused test surface against the resolved base and branch, obtain independent general-code and test-economics approvals of the exact final diff, and record the verdicts for the pull-request handoff.
