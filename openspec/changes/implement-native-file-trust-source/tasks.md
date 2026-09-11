## 1. Native provider API and ownership

- [x] 1.1 Add the target-selected `NativeFileTrustSource` actor and register it in the stdlib manifest; verify the exact three supported target IDs and actual Wasm/no-libc member reachability failures.
- [x] 1.2 Implement validated, independently owned root and `Path` construction with checked combined length and typed configuration/allocation failures; verify runtime caller mutation/release and construction-time allocation refusal.

## 2. Bounded acquisition and atomic publication

- [x] 2.1 Implement reopen-per-load bounded reads with reusable 4096-byte scratch, exact remaining budgets, and the separate one-byte EOF probe; verify exact offered slices for short, exact, over, and zero-budget outcomes.
- [x] 2.2 Implement stage-preserving open/read/close translation and primary-error cleanup precedence; verify receiver-controlled read/close, limit-plus-close failures, and exact-once consuming cleanup through the existing native-filesystem conformance seam.
- [x] 2.3 Decode only complete successfully closed input through `TrustSnapshot.fromPem` and implement `TrustSource`; verify duplicate retention, malformed atomic failure, rename reload, failed-reload retention, and provider-drop independence.
- [x] 2.4 Bound nested cleanup and provider-owner specialization by the exact selected cleanup plan and strict ordinary-type descent; verify both positive paths and continued rejection of unrelated polymorphic recursion.

## 3. Deterministic evidence and target matrix

- [x] 3.1 Add or reuse deterministic synthetic CA PEM fixtures and a checked generator from the named catalog entry; verify canonical PEM bytes and source/DER hashes reproduce without reading a live trust store.
- [x] 3.2 Consolidate ownership and target evidence into existing test workers and add one compact real temporary-directory native fixture for open/read, atomic rename, missing, denied, wrong-kind, and symlink behavior; retain stubs only for irreducible faults and record unavailable runner gaps.
- [x] 3.3 Add the provider to pull-request CI native selection without adding a per-feature determinism suite; verify workflow configuration selects the focused native case.

## 4. Documentation and generated surfaces

- [x] 4.1 Document the public actor, explicit Debian and Darwin file examples, root/symlink model, limits, reload semantics, and lack of OS trust-policy equivalence; verify documentation examples compile.
- [x] 4.2 Regenerate standard-library embeddings, module pages, and catalog/index artifacts from their owning generators; verify generator checks and stdlib manifest checks pass.

## 5. Verification and review

- [x] 5.1 Run strict OpenSpec validation and focused local typecheck, format, lint, behavior, ownership, target-selection, documentation, and conformance checks in the required order; record commands, timings, fixture identities, and runner gaps.
- [ ] 5.2 Commit and push the exact issue diff for CI, then obtain independent correctness and distinct `TEST_REVIEW.md` approval against the actual stack parent; address findings and repeat affected evidence.
- [ ] 5.3 Confirm exact-head CI supplies the explicitly delegated full `pnpm test`, `pnpm check`, and release-candidate evidence before implementation handoff.
