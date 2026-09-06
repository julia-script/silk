# Verification

All required gates passed in order on final source head `a1bc63f3`, integrated above origin/main
`0ee2ed40`:

- `pnpm typecheck`: 18 successful tasks.
- `pnpm format:check` and `pnpm lint`: passed.
- `pnpm test`: 22 successful tasks, including 2,343 compiler tests in 211 files, 321 native acceptance
  cases, 159 LSP tests and 89 CLI tests. The complete compiler/native run took 21m 37.517s overall;
  native acceptance actually executed for 943.863s. The final LSP-only integration rerun reused that
  unchanged successful compiler/native task and ran the affected LSP/docs/editor tests again.
- `pnpm check`: passed, including all 17 repository script/workflow policy tests.
- `pnpm release:candidate`: all 10 packed-artifact checks passed.

Logs are `/tmp/silk-stack-typecheck.log`, `/tmp/silk-stack-format-check.log`,
`/tmp/silk-stack-lint.log`, `/tmp/silk-stack-test-complete.log`, `/tmp/silk-stack-test-handoff.log`,
`/tmp/silk-stack-check-handoff.log` and `/tmp/silk-stack-release-handoff.log`. The final conformance
reruns passed all six foreign-call, twelve artifact-root and four assembly lanes. Artifact-root
and assembly results are identical to the committed JSON records. GitHub CI is separate from these
completed local gates; the current code revision's CI run is pending at the time of this record.

The designated native runner passed all four Linux x86-64/ARM64 debug/optimized lanes with LLVM
22.1.8, the pinned GNU compiler/container images and verified UAPI/header hashes. `results.json`
records object/source hashes and actual naked instructions. Each lane verifies both textual LLVM
IR and bitcode, emits and disassembles a target object, and compiles independent C fixtures.

The fixtures prove fixed/tied registers across seven operands, raw pointer input and result lanes,
C-observed memory writes and reads, getpid using the UAPI syscall number compared with libc, and
terminal assembly caught by a C SIGILL/SIGTRAP handler. A separate executable uses the naked symbol
as its actual ELF entry and passes the incoming loader stack to an independent C probe; the probe
checks argc/argv and exits 23. Disassembly requires the authored stack-forwarding instruction first
and excludes compiler prologues, spills, probes or entry wrappers. This is a machine contract test,
not the downstream no-libc startup/allocator/runtime implementation. LTO is rejected explicitly.

The first debug object exposed a compiler-created branch before the naked fragment. Naked bodies
now emit one LLVM block directly. Another fixture exposed elimination of a terminal side-effecting
assembly operation when its LLVM call claimed memory(none). Observable effects now occupy LLVM's
inaccessible-memory lane, as in the pinned Rust lowering; data memory effects remain independent.
The runner catches the expected trap signal and exits 74, avoiding expensive container core dumps.
Textual IR verification also exposed an invalid `other:` memory spelling: the LLVM renderer now
expresses default memory effects and target-state exclusions consistently with bitcode decoding.

Assembly metadata never receives runtime layout or emitted static storage. Source analysis tests
cover literal and lane validation, fixed/tied conflicts, clobbers, malformed templates, static and
unsupported target admission, machine body restrictions and unavailable instrumentation/unwind.
MIR independently validates normalized contracts, terminal region shape and naked bodies.
Unit type arguments now parse and resolve in call generic lists, enabling the unit result form.

The first integrated full gate exposed an initialization cycle in CLI builds: source declaration
validation imported MIR planning, which reached instance-key initialization before declaration
collection completed. A single-file CLI build reproduced the failure. Source contracts now have
no MIR runtime dependency; `NativeAssemblyPlanning` owns retained-operation and profile validation.
The rebuilt compiler passes the original CLI reproducer and all 89 CLI tests.

The integrated compiler suite also caught a stale byte-for-byte ModuleSurface expectation. The
canonical declaration encoding deliberately includes the optional native machine contract; the
golden now accounts for that field. All 30 ModuleSurface tests pass.

All four ticket changes pass strict OpenSpec validation. Repository-wide strict validation reports
152 passing items and two pre-existing failures: `add-lifetime-values-and-partial-moves` omits three
existing suspension scenarios, and `allow-ordinary-match-arm-blocks` omits the existing owned-place
refinement scenario. Both change directories and their corresponding base specifications are
unchanged from origin/main `0ee2ed40`. These unrelated deltas were not modified.

Submitted through `gh stack` as draft PRs #362 (JUL-122), #363 (JUL-124), #364 (JUL-125) and #365
(JUL-135), based on origin/main `0ee2ed40`. No PR has been merged.

CI twice exposed cold language-server analysis failures after the compiler/native shards passed.
A Linux Node 24 probe constrained to half a CPU reproduced healthy worker retirement during
startup. Startup had incorrectly shared the two-second retirement deadline; it now has an
independent ten-second bound. Workspace analysis reports completed configuration, catalog and
project phases to renew the no-progress watchdog only after actual progress. The same constrained
probe now keeps one worker epoch across four document revisions and returns the expected allocator
inlay hint. A virtual-clock regression proves that startup may outlive the retirement deadline,
and existing workspace/worker tests verify phase reporting and forwarding. The stdio recovery test
uses the production no-progress lease and checks the wedged project's failure and recovery without
requiring healthy compilation to beat a wall-clock deadline.

The first release-candidate run passed nine checks and rejected the expected export inventory's
ordering. All six added actors were present; their expected paths are now in canonical sorted order.
