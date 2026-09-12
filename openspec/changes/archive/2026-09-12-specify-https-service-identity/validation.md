# JUL-169 validation

The design-only record below is historical. Julia subsequently authorized implementation of all
created specifications; the current runtime evidence follows.

## Runtime implementation

Implemented `silk.https_identity` and `silk.certificate_identities`, including registered public
APIs, generated reference pages, and the JUL-183/JUL-184 follow-through. Neither module validates
certificate trust, parses an HTTPS URI, or implements TLS. Opaque schema-dependent ASN.1 values
retain the documented framing-versus-schema boundary in design.md.

Focused execution on the runtime change:

| Check                                                      | Result                                                                                                                   |
| ---------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| Matcher native corpus `https-identity-matrix-v1`           | Passed; 117 matrix cases plus construction/budget checks, 151.80 seconds test body / 163.78 seconds wall                 |
| SAN native corpus `https-san-adapter`                      | Passed; GeneralNames cases and four certificate-envelope integration cases, 21.93 seconds test body / 27.26 seconds wall |
| Borrow-contract semantic test in `BytesAcceptance.test.ts` | Passed; one shared analysis snapshot, 4.35 seconds test body / 8.11 seconds wall                                         |
| Existing manifest-order and namespace-discovery tests      | Passed; two tests, 54 milliseconds test bodies                                                                           |
| `pnpm typecheck`                                           | Passed; 18 tasks, 55.603 seconds                                                                                         |
| `pnpm release:candidate`                                   | Passed; 11 build tasks and all 10 package-consumer tests, 50.69 seconds test-run wall time                               |
| Strict OpenSpec validation                                 | Passed                                                                                                                   |
| Full reference generation and documentation policy         | Passed; 105 modules, no policy violations; final adapter prose refreshed through the same generator                      |

Independent correctness review approved both runtime modules after malformed nested primitive
handling was corrected. Dedicated test-economics review approved two consolidated native programs
and one semantic snapshot. The added measured test bodies total 178.08 seconds across separate
focused runs under variable shared-host load. The design base has none of these test bodies; this
is not a controlled whole-suite wall-time delta. Baseline runner startup comparison was unavailable
because the isolated checkout could not load equivalent dependency/runner instances. No test
timeouts, assertions, runner settings or additional backends were added.

Initial fixture attempts exposed generic-lifetime/lowering limitations; the final SAN fixture uses
a certificate-owning wrapper and explicit borrowed descriptor lifetimes. Earlier oversized literal
fixtures exhausted the compiler heap; runtime-built boundary data and smaller helper functions
replaced them. The passing runs use the default heap and one worker.

Final formatting, lint, package-candidate and published-head evidence are recorded in PR #406.
The full repository test/check gate was not repeated for this runtime follow-through; focused
passes do not establish a full repository gate. Current CI status belongs to the published runtime
head, not the historical successful design CI below.

## Historical design-only verification

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
