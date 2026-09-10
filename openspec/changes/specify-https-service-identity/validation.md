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

## Local repository verification

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

## CI and remaining gate

[CI for the reviewed design commit](https://github.com/julia-script/silk/actions/runs/34530688744)
passed validation, all four compiler shards, documentation, LSP/CLI, native smoke, macOS native OS
and WebContainer browser jobs. The platform-supply job was skipped by its workflow condition.
The draft PR and Linear record hold the exact final head SHA and current CI status after this
verification-record update.

Required local verification remains unresolved. Preserve the draft and resume the repository gate
when the host has sufficient isolated capacity; do not mark JUL-169 In Review or claim the complete
workflow has passed. The test-economics reviewer advised against another full run under unchanged
contention. No runtime support is claimed by this design or its verification.
