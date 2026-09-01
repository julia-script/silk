## 1. StackPath Evidence Gate

- [ ] 1.1 Characterize the evaluator's existing unhandled-failure logical path at an inner closed Effect boundary and verify exact outer-to-inner canonical frame order, source spans, activation-limit bounds, and current cleanup order with a focused test.
- [ ] 1.2 Prototype transfer of the existing immutable path snapshot into one owned opaque outcome whose escaping affine failure payload owns a distinct cleanup witness; verify exactly-once frame and payload cleanup, no retained failure value, no second unbounded path copy, and unchanged trap behavior, then verify drop, move-through-case, downstream-failure, and repeated-invocation routes reclaim each snapshot exactly once; stop implementation and return SLP-0004 to Candidate if any condition fails.
- [ ] 1.3 Record the representation decision and evidence in a checked-in findings note and verify it cites the focused characterization and explicitly closes or triggers the SLP revisit condition.

## 2. Marker and Eligibility

- [ ] 2.1 Add lossless contextual `test` marker syntax and local recovery, then verify parser, syntax-correspondence, and formatter cases preserve trivia, parse exact `pub test effect fn` and `test fn` forms for semantic rejection, and keep `test` usable in ordinary identifier positions.
- [ ] 2.2 Publish the marker on canonical indexed function headers and validate private named top-level non-generic zero-parameter closed Effect-to-unit eligibility; compare marked and unmarked equivalents to verify unchanged body resolution, visibility, callable and declared rows, ordinary calls, HIR, and MIR, then verify one accepted arbitrary typed-failure case and distinct rejected public, ordinary, generic, parameterized, non-unit, and residual-requirement cases.
- [ ] 2.3 Add stable declaration-local test-eligibility diagnostics and verify codes, primary spans, canonical ordering, a parser/header-caused unavailable contract retains only its cause, and absence of speculative or invalid partial inventory entries.

## 3. Closure and Inventory

- [ ] 3.1 Compose existing one-root CompilationRequest loads through a role-aware ProjectRequest and verify ordinary requests remain one-root, overlapping test/runner roots load each canonical module once, reordered roots yield identical facts, runner-only modules are distinguished, and missing roots/imports retain partial causes without a runnable inventory.
- [ ] 3.2 Build the immutable test-only inventory ordered by canonical module identity then declaration source order and verify unrooted sources are not scanned, shared declarations appear once, runner-only declarations remain excluded, semantically or ownership-invalid test/runner bodies retain diagnostics but publish no runnable inventory, and an ordinary build roots no inventory, adapter, runtime, or test-only code-size cost.
- [ ] 3.3 Add opaque Copy ordinal handles, borrowed inventory access, and borrowed canonical UTF-8 ID metadata; verify exact `canonical/module::declarationName` bytes without allocation/copying, same-module source ordering, and ordinary iteration while construction, representation access, direct calls, and general callable conversion remain unavailable.

## 4. Invocation and Path Inspection

- [ ] 4.1 Generate one direct closed invocation adapter per eligible declaration only for test compilation and verify heterogeneous typed failure rows use the uniform handle operation without a source-callable erased function pointer or ordinary-build rooting.
- [ ] 4.2 Map normal return and unhandled typed failure to Passed and Failed and verify successful and internally recovered paths clean affine state exactly once before Passed, an affine application-failure payload cleans exactly once and exposes no value before Failed, each selected handle runs once, traps remain fatal, and every other existing evaluator termination remains outside Outcome with its prior classification.
- [ ] 4.3 Add immutable checked StackPath length/frame inspection over the proven owned snapshot and verify custom consumption sees every outer-to-inner canonical logical frame and source span, out-of-range lookup returns checked absence, repeated paths reclaim exactly once, and source cannot construct or mutate a path.
- [ ] 4.4 Declare inventory, metadata, invocation, and path inspection evaluator-only in the intrinsic availability catalog and verify every evaluator-supported semantic target is admitted, reachable native/direct-Wasm emission is rejected before lowering, unreachable wrappers retain no test symbols, and repeated planning is byte-identical.

## 5. Privilege and Verification

- [ ] 5.1 Compile and evaluate same-named and renamed Test, Reporter, AssertionError, StandardRunner, and equalBytes fixtures while auditing the intrinsic catalog and semantic, HIR, MIR, evaluator, and backend branches; verify ordinary behavior is identical and every testing-specific branch is limited to marker, inventory, metadata, opaque invocation, or path inspection.
- [ ] 5.2 Add committed deterministic facts and encoding goldens and verify repeated in-process analysis of reordered roots produces byte-identical inventory and diagnostics without adding a redundant fresh-process determinism test.
- [ ] 5.3 Run focused syntax, semantic, closure, evaluation, cleanup, and privilege tests, then `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`; record every exact result and identify any pre-existing failure before handing off the standard-library slice.
