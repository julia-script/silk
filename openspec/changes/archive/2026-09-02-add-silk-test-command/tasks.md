## 0. Platform Evidence Gate

- [ ] 0.1 Before implementing CLI forwarding, enumerate every supported OS/runtime argument boundary, invoke the real executable with representative admitted non-NUL byte sequences, and compare bytes at the post-parser/pre-HostInput seam; record each platform's admitted byte domain and return SLP-0004 to Candidate if any admitted byte is normalized.

## 1. Project Test Configuration

- [ ] 1.1 Extend manifest decoding and Project facts with optional `[test]`, required nonempty roots when present, and optional runner; verify defaults, ordered and repeated explicit values, malformed tables, empty roots, wrong types, and exact typed error details.
- [ ] 1.2 Resolve user test and runner entries from the manifest directory, check containment and exact `.silk` spelling against the source root, and derive identity relative to that root; verify the `source-root = "src"` plus `src/tests/x.silk` case, escape rejection, absent and unreadable files, canonical first-load de-duplication, runner/test role separation, invalid main shapes/body diagnostics, and that an absent runner loads unshadowable shipped `silk/test_runner`.
- [ ] 1.3 Add the deterministic standard-library test-root catalog to the shipped-source generation path and jointly gate it with the shipped-source table; verify repeated generation is byte-identical, uncataloged files are not scanned, entries resolve through the same canonical identity rules, and missing, corrupt, or mismatched catalog state fails operationally before analysis.

## 2. Test Workflow

- [ ] 2.1 Register `silk test [filter ...]` and `silk test --standard-library [filter ...]`; assert the exact root subcommand list retains existing commands and removed compile behavior, root help states test's purpose, and test help exposes only `--manifest-path`, `--standard-library`, variadic filters, and `--`, rejecting engine/backend/target/profile/release/watch controls and conflicting standard-library plus manifest selection.
- [ ] 2.2 Add a TestWorkflow actor that selects either project TestConfiguration or the toolchain catalog, composes test and runner roots, completes recoverable analysis once for the ordinary host target, and verify standard and every admitted custom entry shape evaluate exactly once while absent roots, invalid closures, entry diagnostics, or eligibility diagnostics prevent execution; include a runner-only marked declaration and a runner also explicitly designated as a test root.
- [ ] 2.3 Seed the existing evaluator HostInput script with unchanged platform program-name bytes at index zero plus unchanged filter byte arrays; verify exact index-zero bytes for standard and custom runners without a stable spelling promise, filter order, zero and multiple filters, option-looking filters after `--`, arbitrary seam-injected non-UTF-8 bytes, and ordinary OsHostInput/Allocator lexical provision without a test-only entry adapter.
- [ ] 2.4 Add a scoped evaluator StandardStreams boundary that writes runner standard-output bytes to command stdout once and in order; verify exact PASS/FAIL/frame/summary bytes, no transcript replay, host write failure flows through ReportError to standard status 2, later cases stop, and boundary resources close on success, typed failure, report failure, interruption, and every represented evaluator termination.
- [ ] 2.5 Preserve standard-runner statuses 0/1/2, canonical custom entry termination including an arbitrary i32 status, and structured pre-execution source/configuration/storage plus every existing non-entry-completion evaluator class; verify absent configured files and catalog damage return 2, absent imports and diagnostics return 1, mixed source/operational failure returns 2, and no failed pre-execution path invokes a runner.

## 3. Root and Filter Acceptance

- [ ] 3.1 Add user fixtures for absent `[test]`, present roots without runner, repeated and multiple explicit roots, overlapping imports, an unrelated source file, a distinct custom runner, invalid runner entries, a runner-only marked declaration, and a runner also named as a test root; verify inventory membership, canonical order, runner isolation, and single loading exactly.
- [ ] 3.2 Add command fixtures for user-project and standard-library modes, no filters, ASCII-mixed-case filters, overlapping multiple filters, no match, selected failure, reporting infrastructure failure, arbitrary custom status, every existing non-entry-completion evaluator classification, and trap; verify catalog consumption, selected IDs, single invocation, exact stdout order, cleanup, and final command behavior.
- [ ] 3.3 Add regressions proving `[test]` never changes ordinary build/check/run roots, statuses, artifacts, or `run --` argument behavior and that manifest build backend, target, and profile settings never change the evaluator-only test target.

## 4. Verification

- [ ] 4.1 Run focused project, source-entry, catalog, CLI parsing/help, platform-byte, HostInput, StandardStreams, evaluator workflow, status, regression, and acceptance tests, then `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`; record every exact result and identify any pre-existing failure before handing off sufficiency evidence.
