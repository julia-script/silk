## 1. Native provider API and ownership

- [x] 1.1 Add the target-selected `NativeFileTrustSource` actor and register it in the stdlib manifest; verify supported-profile imports resolve and Wasm/no-libc member reachability is rejected.
- [x] 1.2 Implement validated, independently owned root and `Path` construction with checked combined length and typed configuration/allocation failures; verify invalid-root and ownership cases through structured analysis and focused execution.

## 2. Bounded acquisition and atomic publication

- [x] 2.1 Implement reopen-per-load bounded reads with reusable 4096-byte scratch, exact remaining budgets, and the separate one-byte EOF probe; verify short, exact, over, and zero-budget outcomes.
- [x] 2.2 Implement stage-preserving open/read/close translation and primary-error cleanup precedence; verify receiver-controlled read/close failures and exact-once consuming cleanup through the existing native-filesystem conformance seam.
- [x] 2.3 Decode only complete successfully closed input through `TrustSnapshot.fromPem` and implement `TrustSource`; verify duplicate retention, malformed atomic failure, rename reload, failed-reload retention, and provider-drop independence.

## 3. Deterministic evidence and target matrix

- [x] 3.1 Add or reuse deterministic synthetic CA PEM fixtures and record generation commands, source/DER hashes, and expected results; verify hashes reproduce without reading a live trust store.
- [x] 3.2 Add consolidated ownership, target-selection, host-native, and conformance coverage at the cheapest falsifying tiers; verify supported debug/optimized target lanes where runners exist and record unavailable runner gaps.
- [x] 3.3 Add the provider to pull-request CI native selection without adding a per-feature determinism suite; verify workflow configuration selects the focused native case.

## 4. Documentation and generated surfaces

- [x] 4.1 Document the public actor, explicit Debian and Darwin file examples, root/symlink model, limits, reload semantics, and lack of OS trust-policy equivalence; verify documentation examples compile.
- [x] 4.2 Regenerate standard-library embeddings, module pages, and catalog/index artifacts from their owning generators; verify generator checks and stdlib manifest checks pass.

## 5. Verification and review

- [x] 5.1 Run strict OpenSpec validation and focused local typecheck, format, lint, behavior, ownership, target-selection, documentation, and conformance checks in the required order; record commands, timings, fixture identities, and runner gaps.
- [ ] 5.2 Commit and push the exact issue diff for CI, then obtain independent correctness and distinct `TEST_REVIEW.md` approval against the actual stack parent; address findings and repeat affected evidence.
- [ ] 5.3 Confirm exact-head CI supplies the explicitly delegated full `pnpm test`, `pnpm check`, and release-candidate evidence before implementation handoff.
