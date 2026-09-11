# JUL-169 validation

Scope: design-only `specify-https-service-identity`; verified work base
`c6eae2f976a8f1a7681ffbfdcbbc8c776b9f0c96` was clean. The diff changes eight OpenSpec files only.
No runtime, manifest, package exports, test configuration or executable tests change. The fixture
matrix is a specification for JUL-183, not delivered runtime support.

## Design and review

- `openspec validate specify-https-service-identity --strict`: passed, including after review fixes.
- `openspec status --change specify-https-service-identity`: all four planning artifacts complete.
- Independent design review: approved after correcting unit-union syntax and separating invalid
  IA5 bytes from ordinary DNS syntax in wildcard processing.
- Dedicated test-economics review: approved the committed design diff at
  `a6507c2474505917388c3aa97f7f35d557f5141f`. Eight prose/configuration artifacts add no executable
  test inputs; inferred incremental default-test cost is zero seconds. Strict validation measured
  1.031 seconds wall time. The final metadata-only commit receives a separate confirmation in the
  PR and Linear record.
- [JUL-183](https://linear.app/juliaortiz/issue/JUL-183) is the separately scoped five-point runtime
  follow-up, created in canonical-project Triage and read back successfully.

## Initial local repository verification

Commands ran in the required order on the published design commit:

| Command             | Result                                                                                        |
| ------------------- | --------------------------------------------------------------------------------------------- |
| `pnpm typecheck`    | Passed; 18 tasks successful                                                                   |
| `pnpm format:check` | Passed; 3,615 files                                                                           |
| `pnpm lint`         | Passed                                                                                        |
| `pnpm test`         | Failed after 37m00s: compiler 2,437 tests passed, 12 timed out across eight files             |
| `pnpm check`        | Failed in LSP tests: 147 passed, two timed out; preceding formatting/build/lint stages passed |

Compiler failures were eleven 60-second timeouts and the 120-second `fibers.md:16` documentation
example timeout. No assertion mismatch was reported. All twelve subsequently passed in focused
reruns with `--maxWorkers=1` and unchanged timeout/assertion settings:

- `NumberText.test.ts`, selected diagnostic-span test: one passed, seven unselected; 60.93 seconds
  total, 33.42 seconds assertions.
- The eleven other failed tests selected across `VectorAcceptance`, `Driver`, `CoroutineFrame`,
  `CompilerArtifactArchitecture`, `EffectSuspensionNative`, `Logging` and `DocumentationExamples`:
  eleven passed, 46 unselected; 395.35 seconds total, 360.57 seconds assertions.

The LSP failures were `AutoImportScale.test.ts`'s header-only lookup test (60-second timeout) and
`Server.test.ts`'s real-stdio test (120-second timeout). Focused LSP rerun outcomes are recorded in
[the draft PR](https://github.com/julia-script/silk/pull/406). The failed full commands remain failed;
focused passes do not replace a successful repository gate. The compiler native-acceptance phase
and root script tests were not reached by those failed local commands.

Concurrent Vitest workers from another checkout were observed on the shared host. Unchanged
executable inputs, successful focused reruns and successful CI support contention as a hypothesis;
a clean-base reproduction was not run, so these failures are not asserted to predate this change.
No timeouts, assertions, production code or runner settings were changed to obtain a pass.

`pnpm release:candidate` is not applicable because no package contents or exports change.

## Initial CI

[CI for the reviewed design commit](https://github.com/julia-script/silk/actions/runs/34530688744)
passed validation, all four compiler shards, documentation, LSP/CLI, native smoke, macOS native OS
and WebContainer browser jobs. The platform-supply job was skipped by its workflow condition.
The draft PR and Linear record hold the exact final head SHA and current CI status after this
verification-record update.

## Resumed verification and base correction

The user resumed verification after final metadata head `a11fe0e4` passed all applicable
[CI jobs](https://github.com/julia-script/silk/actions/runs/34533767397). Typecheck, formatting and
lint passed again. Default-worker LSP passed all 149 tests; a different compiler layout test then
timed out at 60 seconds, and that run was stopped.

A scheduling-only retry used `VITEST_MAX_WORKERS=2`, temporarily passed through Turbo's strict
environment. The wrapper restored `turbo.json` byte-for-byte afterward. No suites, assertions or
timeouts changed. All 209 compiler files and all 2,449 tests passed in 1,263.33 seconds. Native
acceptance then exposed malformed source in existing corpus programs (`silk.i32n`, missing
`pub fn`, and similar corruption), producing `Rejected` instead of `Compiled`. The obsolete-base
run was stopped after those concrete failures; it was not a successful full `pnpm test`, and its
chained `pnpm check` was not reached.

These malformed corpus fixtures predate JUL-169: the task diff never changes them, and they are
present at the initial work base. Fix commit `333f76c70f35a9f9e43c313005a053884e84f0e2` subsequently
landed in main through PR #407. The task branch was rebased onto
`991386ae75fe3037e70da1cde9dc71d91dbc3e67`, incorporating that upstream fix without adding it to
the identity PR's scoped diff. The same upstream change delivers raw certificate-extension views;
the design now distinguishes that API from the still-deferred GeneralNames/SAN adapter.

## Successful verification on the corrected base

Verification completed on published design head `1842da7f68f1b0134f58faa61fcb887e257293c6`,
based on `991386ae75fe3037e70da1cde9dc71d91dbc3e67`:

| Required command    | Final result and execution evidence                                                                                      |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `pnpm typecheck`    | Passed: 18 successful tasks, two cache hits, 18.276 seconds                                                              |
| `pnpm format:check` | Passed: 3,627 files, 4.694 seconds                                                                                       |
| `pnpm lint`         | Passed                                                                                                                   |
| `pnpm test`         | Passed: 22 successful tasks, 17 cache hits, 7m13.892s in the final invocation                                            |
| `pnpm check`        | Passed: formatting and lint, 11 cached build tasks, 33 cached typecheck/test tasks, and 19 freshly executed script tests |

The fresh compiler execution used `VITEST_MAX_WORKERS=4`: all 209 compiler files / 2,451 tests
passed in 645.50 seconds; all 335 native acceptance tests passed in 2,906.07 seconds. The same
compiler task passed documentation policy for 103 modules and all 57 doctests. Its successful
Turbo result was reused by the final invocation, not rerun or fabricated. CLI and the remaining
package tests also passed; the final invocation freshly completed LSP and the outstanding
editor-support, editor-extension and documentation tasks.

LSP's four-worker run had five timeouts across four files. The first one-worker retry passed
147/149 tests; the stdio file still had one 120-second timeout and one healthy-worker retirement
assertion failure while other compiler work and substantial host swapping were observed. After
the competing documentation generator finished, the full one-worker LSP retry passed all
11 files / 149 tests in 283.60 seconds, including every previously failing case. The original
stdio case took 43.544 seconds within its unchanged 120-second limit. These failed attempts are
retained as evidence; successful scheduling-adjusted verification does not establish that default
scheduling succeeds on the shared host or prove a unique cause for every earlier failure.

Both worker settings were local environment overrides passed through Turbo's strict environment
by temporarily adding `VITEST_MAX_WORKERS` to `globalPassThroughEnv`. This also overrides the
native target's explicit worker setting, but that target selects one file and still executes its
cases sequentially. No suites, assertions or timeout settings changed. Each wrapper restored
`turbo.json` byte-for-byte; its working-file hash and committed blob both equal
`baea0e2932e61540aef28fcb032da66b01ffc3b1`. The final repository diff includes no runner changes.

Independent design and dedicated test-economics reviewers approved the exact corrected-base
design diff. The test-economics reviewer accepted this explicitly reported scheduling method and
confirmed zero added executable test work. All applicable CI jobs passed on design head
`1842da7f`; platform supply was conditionally skipped. The PR and Linear handoff record hold the
exact final metadata head, its review confirmation and current CI status after this record update.

The local verification gate is resolved. The final metadata-only update is checked with strict
OpenSpec validation, formatting and diff checks; executable inputs are unchanged. No runtime
identity support is delivered by this design. JUL-183 remains the pure matcher follow-up and
JUL-184 the separately scoped SAN-adapter intake.
