## 1. Seeded Random Witnesses

- [ ] 1.1 Add a closed standard-library test that constructs the documented seeded Random provider inside its body and verifies the published first scalar through Test.assert; verify `silk test --standard-library` catalog discovery and a passing default report.
- [ ] 1.2 Add a closed standard-library test that fills bytes from the documented seed and compares the existing published vector through Test.equalBytes; verify the expected vector has one committed source of truth and no host-language comparison loop remains.
- [ ] 1.3 Add a representative manifest-rooted user fixture that repeats both the scalar Test.assert and byte-vector Test.equalBytes operations and verify it uses the identical eligibility, inventory, invocation, assertion, and reporting surface.

## 2. Customization and Edge Evidence

- [ ] 2.1 Add one runnable custom-runner fixture with a shared mutable counting Reporter and public Event inspection; verify every reached passed or failed case updates state exactly once, complete StackPath frames remain inspectable, the runner derives one exact nonzero i32 status from Reporter state, and the command preserves it.
- [ ] 2.2 Add a custom runner that deliberately omits reporting and verify invocation and aggregate outcomes need no Reporter provider and remain deterministic.
- [ ] 2.3 Add one connected fixture whose module is both custom runner and explicit test root and that imports a runner-only helper; verify its private marked test enters through the test-root role, runner-only declarations remain excluded, overlapping roots de-duplicate, canonical order holds, and each case reached before termination runs once. Cite prerequisite unit evidence instead of duplicating isolated root tests.
- [ ] 2.4 Cover ASCII-mixed-case OR filters through the command, a non-ASCII ID with exact positive match and normalization/case-fold near-miss, and an invalid-byte filter ORed with a matching ASCII filter at the post-parser HostInput seam; verify exact selection/order without decoding and cite the platform-byte preservation gate.
- [ ] 2.5 In compact connected fixtures, cover all-pass status 0, selected-failure status 1, no-match status 2, ReportError overriding prior case status and reclaiming the current Event/path while stopping later cases, source rejection before any runner invocation, and fatal trap termination without Failed or 0/1/2 fabrication; cite isolated prerequisite status and cleanup cases rather than rerunning them.
- [ ] 2.6 Compare standard and custom reporting for one connected nested Random-helper failure and verify the exact standard omission predicate while the custom Reporter observes the identical complete owned StackPath; reuse the prerequisite presentation golden for isolated frame-order claims.

## 3. Sufficiency and Privilege Findings

- [ ] 3.1 Rename equivalent Test wrapper, Reporter, assertion, filter, and runner actors in one focused connected fixture and verify semantic facts and evaluator behavior remain unchanged, including owned path capture and checked path inspection.
- [ ] 3.2 Inventory syntax, semantic, HIR, MIR, evaluator, intrinsic, backend, and command artifacts and verify every testing-specific branch is limited to marker, inventory, metadata, opaque invocation, owned logical-path capture, or checked logical-path inspection with no actor-spelling privilege; cite unchanged prerequisite backend evidence or inspect the final backend artifacts.
- [ ] 3.3 Write the checked-in SLP-0004 findings report mapping every goal and falsifier to evidence, recording the StackPath and platform-byte gate results, classifying every discovered wall and disposition, and confirming the complete deferred list remains outside this slice: future SHA, generic equality and broader comparison, rendering/messages/callsite events, skip/tag/advanced filters, shuffle/retry/snapshot/fuzz/coverage/watch, source-visible target/build configuration, compiled/matrix modes, concurrency/process isolation, and recoverable traps. Mark the finding non-passing for any unproven falsifier, failed gate, undisposed wall, or actor-spelling privilege, and return the SLP to Candidate where its accepted revisit rule applies.

## 4. Verification

- [ ] 4.1 Run the focused seeded-Random, user/stdlib catalog, custom-runner, Reporter, filter, status, path, trap, and privilege suites, then `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`; record every exact result and identify any pre-existing failure before declaring SLP-0004 implementation-ready.
