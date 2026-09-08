# Implementation evidence and remaining scope

This change is incomplete. None of JUL-129, JUL-134, JUL-136, JUL-130 or JUL-147
is closed by this worktree.

## Raw Linux implementation

The source modules `silk/raw_linux`, `silk/raw_mapping`,
`silk/raw_process_inputs` and `silk/raw_start` implement the synchronous raw subset.
The reserved-root check now verifies selected standard-library roots against the
resolver's exact bytes and origin. A regression test accepts canonical roots and
rejects forged provenance. No-libc object emission rejects retained generated
language-runtime requirements before invoking the toolchain; a focused test uses
an unavailable compiler command to distinguish capability rejection from spawning.

Local conformance passed on x86-64 and ARM64, each unoptimized/debug and optimized,
with LLVM 22.1.8 and pinned Debian images. The independent UAPI fixture compiled
against checked linux-libc-dev 6.1.180-1 header hashes. Final artifacts were static
ET_EXEC files with no interpreter, dynamic dependency or undefined symbol. Both
architectures executed with expected status and bytes. Reproducible commands and
coverage are in `packages/compiler/conformance/raw-linux/README.md`; full local
reports and disassemblies are under `.scratch/raw-linux-conformance`.

Required CI wiring has been added; CI execution has not been observed. Raw catalog
admission and local slice evidence are recorded; repository verification results are below.
Generated documentation now includes no-libc Linux profiles and documentation
policy checks every admitted documentation profile.

## Storage migration inventory

`CoroutineRuntime.ts` still emits malloc/free, thread-local byte accounting and
SILK_PRIVATE_EXECUTION_STACK_LIMIT_BYTES parsing. `NativeProgram.ts` declares
push(size, alignment) and pop(frame). `NativeFunction.ts` acquires a frame only
when there is no reusable resume frame and traps on null. `NativeSuspension.ts`
releases completed invocation storage. `NativeExecutionOperation.ts` releases
frames during execution cleanup; `NativeAggregate.ts` carries that capability
through cleanup contexts. `LlvmWasmRuntime.ts` still implements the same symbols
using a module heap cursor and non-reclaiming free.

An explicit source-instance state and selected typed contract must replace this
entire shared boundary together. No replacement ABI has been implemented.

## Other migrations

`os_child_process.silk` now delegates to source-owned `native_process.silk`.
Execute/capture intrinsics and generated C/global buffers are removed. All six
real-process target/optimization lanes pass, including startup error versus exit
127, signals, inputs and simultaneous 128 KiB captures. Foreign failure-injection
coverage and final catalog admission remain in progress; see process-admission.md. Hosted entry, reporting and all four host-input intrinsic replacements
are pending. The JUL-147 WS/SPEC closure ledger and all required remainder evidence
are pending; generated documentation updates alone do not satisfy that audit.

## Verification before the process replacement

The following aggregate results predate the process edits. They must be rerun
after all requested migrations; they do not verify the current worktree.

Passed: pnpm typecheck, pnpm format:check, pnpm lint, pnpm test, pnpm check and
pnpm release:candidate. Compiler verification included 2,369 tests in 210 files
and all 323 shared native acceptance cases. Release-candidate validation passed
all ten tests. The required check also passed all 17 repository-script tests.
The four raw target/optimization lanes passed separately.

During verification, an added test initially supplied an unpublished profile;
that was corrected. An overlapping rebuild also caused temporary missing-output
errors in CLI tests; the sequential rerun and final aggregate check passed.
A proposed unsafe modifier on the ordinary source C initializer was rejected by
the foreign declaration contract; it was removed and all raw lanes rerun.
These were introduced during this work, not asserted to be baseline failures.

This verification does not close the unimplemented tickets. Further implementation
must rerun the applicable gates. CI execution has not been observed. The additional
process protocol probes and their limitations are recorded in process-admission.md.

## Process replacement verification

Compiler build and test typecheck passed. The four focused suites (IntrinsicCatalog,
NativeToolchain, CAbi, DeclarationIndex) passed all 151 tests. Real process debug
and optimized lanes passed on Darwin ARM64, GNU ARM64 and GNU x86-64. Reports are
under `.scratch/native-process`. The first direct runner omitted the existing
hosted executable runtime object and failed linkage; adding that required object
fixed all six lanes. The hosted object remains a JUL-130 migration target.

Both real overlapping-invocation and fault modes now pass in all six lanes each.
Fault evidence covers 42 Darwin / 39 GNU foreign-call failure ordinals and all 32
allocation ordinals, preserving descriptor/handle/child ownership and primary
errors. GNU fragmented/truncated/malformed notice cases pass. Independent C
signature, layout and constant assertions pass on all three supplies. The
process catalog decodes, stdlib and integrity checks pass, documentation policy
passes for 80 modules, and generated documentation has been refreshed. GNU
child-side fault injection remains distinct from the completed parent sweep.

## Execution-storage component plumbing

Added a generic RuntimeComponent actor and explicit artifact-composition component
catalog. Bindings are independent of executable entry, canonicalized into the
artifact identity, reject duplicate capability/operation bindings, and round-trip
through Project configuration. Catalog entries do not themselves load modules;
actual capability demand activation and signature verification remain to implement.
The compiler and test TypeScript checks passed, and Instances passed all 34 tests.
Storage state/lifetime admission and replacement lowering remain pending; the old
C storage implementation has not yet been removed. See storage-inventory.md.

## Source storage provider and pointer boundary

The ordinary source module `silk/execution_storage` now owns per-instance
accounting, fixed-address aligned reservations, non-LIFO release, capacity refusal
and state teardown. Its four C exports implement the proposed ABI. Its safe
constructor reports allocation refusal; the component creation adapter translates
that failure to null. State and frame storage use explicitly declared malloc/free,
with no environment knob, thread-local state or compiler-known provider name.

Unsafe `Intrinsic.pointerReinterpret` changes pointee type while preserving
mutability, nullability, extent and address space. Semantic and MIR checks retain
those invariants. It grants no ownership or initialization guarantee. Explicit
nullable qualification is required before constructing the provider's nullable
pointer fields; the implementation does not weaken MIR validation.

All six native ABI lanes and all six native allocation-fault lanes passed locally.
The independent C callers verify state isolation, alignments through 4096,
non-LIFO release and exactly-once cleanup at all five allocation refusal points.
A freestanding Wasm C allocator/caller also passes both optimization lanes with
no module imports. `conformance/execution-storage/README.md` records the commands
and precise scope; CI now requires these probes and retains their artifacts.

Source C calls/exports on Wasm admit only direct address-space-zero data pointers,
32-bit integers and void results. Variadics, callbacks, borrowed foreign arguments,
other scalar widths and indirect calls remain rejected. A Driver-produced source
storage fixture also ran through the existing Wasm allocator and returned 42.

This is provider and ABI evidence only. Demand-driven component activation, typed
binding admission, transfer/package state plumbing, cancellation integration and
deleting the old CoroutineRuntime/LlvmWasmRuntime frame bridge remain required.
The existing full repository checks above predate these edits.

Provider follow-up verification passed: 171 tests across CAbi,
IntrinsicAvailability, Elaboration and IntrinsicCatalog; TypeScript test-project
checking; standard-library documentation policy and generation; formatting checks
for the affected ABI/test/conformance files; and strict OpenSpec validation.
The full repository verification gate remains pending until the migrations finish.

## Demand-driven component activation

`ExecutionStorageComponent` now selects the four-operation contract from artifact
configuration after concrete MIR requires private frames. It resolves ordinary C
exports by the configured source selector and checks nullable mutable byte pointers,
`usize` arguments, void results and non-variadic, returning, non-borrowing contracts.
A selected export whose own implementation suspends is rejected as a dependency
cycle. Further bootstrap closure audit must include independently driven Execution
work reachable from a provider; the direct suspension check alone is not that audit.

`Frontend.withComponents` loads demanded modules through the explicit source
resolver while preserving bytes and origins of the existing snapshot. Already
loaded modules need no second frontend pass. Analysis realization and Driver
preparation both perform demand selection before artifact planning. The expanded
frontend supplies source digests, declarations, tooling and cache identity; Driver's
final-stage plan now uses that expanded frontend too. Hosted and Wasm build defaults
offer the source provider; no-libc native defaults offer none. Explicit component
catalogs remain authoritative and unused catalogs retain no source.

`Analysis.realize` now requires the source resolver capability because realization
can discover selected source dependencies. Internal callers, tests and the README
example were updated. The single-source convenience operation supplies its embedded
resolver at its boundary.

Verification passed: 69 focused instance/analysis/stored-executable tests, compiler
build, test-project TypeScript checking and full workspace `pnpm typecheck` (18 tasks).
Tests cover inactive catalogs, default activation, arbitrary export names, changed
provider source identity, missing selection, wrong signatures, direct suspension
cycles, expanded tooling, and Driver preparation. The native frame calls still use
the obsolete push/pop ABI; replacing those with stateful acquire/release and
transfer/package ownership is the next required implementation step.

## Stateful frame lowering

The backend now binds the validated source functions directly. A fifth transfer
word owns lazily acquired state; frame allocation supplies that state to acquire,
and completed frames supply it to release. Transient drivers destroy state before
returning their final result. An owned Execution stores the fifth word in its
InitialContinuationSegment, initializes it to zero, restores it on activation and
publishes it before exposing a parked package to callbacks. Completion consumes
state before package release; cancellation releases semantic frame contents and
frames, then consumes state. An unstarted package performs no state creation or
destruction call. Independent transient drivers initialize the active-owner word
to zero as well.

CoroutineRuntime.ts, its symbol reservations and generated native C policy are
removed. The old frame header/push/pop implementation is also removed from
LlvmWasmRuntime.ts. No operational push/pop plumbing or environment-limit consumer
remains in compiler source or tests. The native exhaustion corpus now selects an
explicit refusing source acquire operation; it no longer depends on process
configuration. The source provider uses a conservative free declaration. Empty C
attribute sets now normalize to absent attributes, so identical declarations can
share allocator symbols without inventing an attribute conflict.

A first emitter probe exposed a forward SSA reference after lazy state creation.
The acquired frame is now published through the existing invocation slot before
branching to previously declared frame blocks, preserving the encoder's ordering
invariant. All following emitted modules verified.

Passed after these changes: 69 ABI/toolchain/native-suspension tests (including one
million suspended recursive frames and resumed retry failure), 16 execution-layout,
transition and suspension-ownership tests, and six selected native corpus cases:
non-LIFO, latched resume, latched destruction, late canceled Wake, explicit frame
refusal and multiple packages. A transient Wasm entry returned 42. Latched
destruction and multiple-package Wasm cases returned 42, and non-LIFO returned 240,
each in debug and optimized builds. Those three source fixtures now also back the
native corpus. Full workspace typechecking passed (18 tasks).

The permanent Wasm conformance runner now retains separate artifacts for all four
lifecycle cases and checks absence of host imports. Source component lifecycle
counting/reentrancy, the complete provider bootstrap-cycle audit, required GNU
compiler-frame lanes, explicit no-libc provider rejection evidence, refreshed
catalogs and full repository checks still remain before JUL-129 can close.

The permanent Wasm runner passed all 14 lanes: two independent C ABI/fault probes,
eight shared lifecycle executions and four explicit source state/frame refusal
traps. Every module has no host imports. Full workspace formatting passed.
The first lint run rejected two implicit JavaScript parameters in the runner;
typed JSDoc and an Effect-based WebAssembly boundary replace those callbacks.
The runner passed all 14 lanes again after that boundary refactor, and `pnpm lint` passed.

Generated documentation was refreshed after the state migration. These checks are
intermediate evidence; the full test/check/release gate remains pending.

## Native compiler-frame lifecycle admission

Extended the required native execution-storage runner with the shared
latched-destruction, multiple-package and non-LIFO fixtures. Each fixture exports
an ordinary C entry which synchronously drives its owned Execution; the foreign
export suspension restriction is unchanged. A separately compiled C receiver
checks the expected result. Explicit source-component bindings resolve the frame
provider, and each lane retains source digest, artifact identity, object,
relocations, disassembly, linked executable and execution result.

All 24 normal lanes passed (four fixtures times debug/optimized times Darwin
ARM64, GNU x86-64 and GNU ARM64). Both GNU architectures executed in the pinned
containers. All six allocation-fault ABI lanes also passed against the current
compiler/provider. Reports are `.scratch/execution-storage/<target>.json` and
`<target>-faults.json`; logs are
`.scratch/process-admission/storage-lifecycle-{amd64,arm64,darwin}.log` and
`storage-current-faults-{amd64,arm64,darwin}.log`.

Targeted Oxlint passed for the changed runner. These are additional focused
conformance results, not current full-suite or release evidence. Bootstrap closure,
compiler-state counting/reentrant probes, remaining process child-side faults,
hosted migration and final audit still remain. No ticket has been closed.

Added `hosted-inventory.md` documenting every remaining generated host-input and
entry/report boundary, semantic metadata to preserve, per-invocation ownership
requirements, and the exact Zig/Rust implementations inspected. It explicitly
leaves the source report ABI and independent hosted C admission incomplete;
no report lowering was changed ahead of that contract.

## Hosted input source implementation and independent admission

Added `silk/native_host_input` with an affine owned argument/environment snapshot.
Unsafe capture copies all foreign input bytes and retains no raw input pointers.
Argument count, indexed prefix copies and first-match environment prefix copies
operate on the snapshot, preserving absent/empty distinctions and unwritten tails.
Cwd lookup owns a fresh source buffer, grows on ERANGE from 256 bytes through 1 MiB,
and releases every attempted buffer. Allocation failures retain OutOfMemoryError;
host failures and contradictory unterminated success return HostInputError.
`host-input-contract.md` records the input lifetime, byte and cleanup contract.

The new required `conformance/native-host-input/run.mjs` uses independently compiled
C declarations/callers with pinned header digests, LLVM 22.1.8, explicit SDK/sysroots,
LTO rejection and actual execution. All 12 lanes passed: normal and allocation/cwd
fault fixtures, debug and optimized, Darwin ARM64 and GNU x86-64/ARM64. Normal probes
mutate original foreign buffers after capture and verify the snapshot stayed intact,
including non-UTF-8 bytes, empty/missing values, duplicate environment names, prefixes
and tails. Fault probes refuse every successful-path allocation ordinal and detect
leaks or duplicate release; cwd probes exercise growth, errno failure, bound exhaustion
and missing NUL on reported success. Reports live in
`.scratch/native-host-input/<target>.json`, with logs
`.scratch/process-admission/host-input-{amd64,arm64,darwin}.log`.

The object check permits the existing malloc/free allocation boundary but rejects
all other generated runtime dependencies. Added the required platform CI invocation,
artifact upload path and `test:native-host-input` script. Generated stdlib and docs
were refreshed. Workspace typecheck (18 tasks), format check (3446 files), lint and
stdlib documentation policy (82 modules) passed. Build and documentation generation
passed. Full test/check/release have not been repeated for this state.

This is not JUL-130 closure: OsHostInput/startup still use the old protocol until the
selected entry/report context is implemented, and those four intrinsics/adapters must
be deleted together with wiring the new owned provider. No compatibility API is
intended. Source report ABI/lowering, explicit startup invocation, Wasm entry migration,
reentrant hosted probes and the final audit remain required. Task 5.2 stays unchecked
until its source implementation is connected and obsolete consumers removed.

## Hosted source-result dispatch prerequisite

Fixed open failure-row inference: an identical row now contributes identity bindings
for its member parameters during open generic inference. Existing conflicting evidence
still rejects the constraint. This was exposed by an ordinary source EntryResult
interface implementation over an Effect result, whose witness binder previously
remained unresolved even though the receiver/operand supplied the same open row.

A source integration regression now realizes integer, unit and fallible Effect
result policies through a phantom `Application<T>` provider. A small source selector
infers the phantom type from a borrowed result; a bound generic call invokes the
selected interface operation. The Effect itself remains a function operand, preserving
its concrete identity instead of storing it behind an erased field. This establishes
a source mechanism for result adaptation without adding an entry intrinsic or
recognizing a library declaration by name. Actual startup/report state wiring and
suspension through an owned Execution are still pending.

Verification: InterfaceWitnessInference five tests passed, related witness/bound-call/
nested-row/conditional-conformance regression 42 tests passed, and the updated
UserInterfaceWitness seven tests passed. Compiler build, workspace typecheck (18 tasks),
format check and lint passed. Logs are `.scratch/process-admission/entry-inference-*`
and `entry-source-dispatch-tests.log`. Full test/check/release remain pending.

## Source C entry and executable operand identity

Removed `main` from private compiler symbol reservations and corrected the reference
reservation inventory. The selected source runtime can now export C main without
using the generated entry adapter. The new hosted-start conformance fixture wraps
application invocation in an owned Execution and drives it from a non-suspending
foreign export. Integer, unit, successful Effect and typed-failure results are
converted by ordinary source interface policies.

Actual emission exposed and fixed two general compiler representation defects:
static interface witness calls dropped executable operand identities during instance
discovery/lowering, and native address storage used erased public Effect types instead
of concrete environment layouts. Witness calls now retain the concrete call instance;
address allocation and lane offsets follow concrete Effect/callable environments.
The source-interface regression now verifies MIR, including an empty failure row.

All 36 pinned native lanes passed (six cases times debug/optimized times Darwin ARM64,
GNU x86-64 and GNU ARM64). The captured-Effect case receives its value from separately
compiled C and checks it when executed, preventing constant folding from masking an
incorrect environment layout. The fixture has no compiler invocation root, generated
silk_main adapter or generated report/input imports. Both GNU architectures executed
in pinned containers. Reports are `.scratch/hosted-start/<target>.json`; logs are
`.scratch/process-admission/source-entry-{amd64,arm64,darwin}.log`.

CAbi 24 tests, witness/stored-Effect regression 34 tests, and updated
witness/stored-callable/stored-Effect/native-suspension regression 20 tests passed.
Compiler build and workspace typecheck (18 tasks) passed. Formatting and lint were
checked, with final refreshed logs under `source-entry-{format,lint}.log`. The new
required CI command and artifact directory are wired as `test:hosted-start`.

This does not finish JUL-130. The fixture is not yet selected by default, has no input
plumbing or reporter, and its external-park callback is explicitly fixture policy.
Owned process inputs, report-context ABI/lowering, default/custom/none migration,
Wasm entry migration, generated adapter deletion and final audit remain required.
Full test/check/release remain pending for the complete implementation.

## Owned portable host-input provider

OsHostInput now owns an explicit NativeHostInput snapshot. Its constructor moves
that value without allocation; argument and variable lookups allocate exactly the
captured length, while absent values require no result allocation. Working-directory
lookup delegates to the source libc boundary. The stateless provider and all four
intrinsic calls from that module have been deleted. The temporary-directory test
caller now explicitly captures its environment before constructing the provider.

NativeHostInput.environmentSnapshot supplies the library-boundary case with no
entry arguments: it copies Darwin's _NSGetEnviron vector or GNU's external environ
object and records zero arguments. Capture requires exclusion of foreign mutation;
no foreign pointer survives. Null vectors produce an empty environment. The source
and independent C declarations use the selected platform boundary. The admission
header inventory now includes Darwin crt_externs.h and each target's stdlib.h.

All twelve refreshed native host-input lanes passed: ordinary inputs and allocation/
cwd fault sweeps, debug and optimized, Darwin ARM64 and both GNU architectures.
The fixture verifies environment-only capture and moves the entry snapshot into
OsHostInput, then checks argument count and copied raw bytes through HostInput.
Logs are input-provider-{amd64,arm64,darwin}.log under .scratch/process-admission.
Workspace typecheck (18 tasks), formatting and lint passed. Documentation generation,
policy and the temporary-directory regression were still running when this entry
was written; final outcomes must be checked before handoff. The expanded header
inventory was written after these runners started, so final evidence refresh must
include those added header hashes.

This is not JUL-130 completion: the obsolete compiler OsCall/intrinsic/generated-C
protocol remains to be removed, and default startup still needs to supply actual
entry arguments through the owned provider. Reporting and full migration/audit remain
pending, as do the final repository test/check/release gates.

## Host-input compiler protocol removed

Deleted all four host-input intrinsic identities/contracts and their checked inventory
entries, OsCall MIR and verification/encoding/inspection/lowering, LLVM declarations,
native call emission, generated C implementation, argument globals and symbol
reservations. Deleted OsRuntime.ts. Generated entry no longer initializes ambient
arguments; the upcoming source entry must explicitly supply the owned provider.
Removed obsolete runtime-symbol parameters from C generation and updated Driver and
all six conformance callers. Updated foreign declaration tests for ordinary C main
and removed assertions against the now nonexistent MIR operation.

Compiler build, workspace typecheck (18 tasks) and all 157 focused tests passed:
IntrinsicCatalog, CAbi, DeclarationIndex, NativeToolchain, Clock and OsRandomRuntime.
All twelve native process-input lanes passed again after deletion and now record the
expanded header pins. Source/reference/test searches found no old host-input symbol,
OsCall or OsRuntime references. The prior temporary-directory regression passed all
three tests; documentation generation and policy (82 modules) also completed.
Formatting found changed caller wrapping and was repaired; final format/lint results
must be checked in remove-input-final-{format,lint}.log. Final whole-repository
verification remains pending with startup/reporting and the full five-ticket audit.

Final removal checks: workspace typecheck passed, format:check passed after repairing
caller/progress wrapping, and lint passed when rerun after the workspace rebuild
finished. The overlapping lint run had reported unresolved downstream types while
build outputs were being replaced; the sequential rerun is clean. Strict OpenSpec
validation and git diff --check passed. JUL-130 reporting/entry migration and the
full five-ticket verification remain incomplete.

## Source entry owns and provides process inputs

The hosted entry fixture now exports the ordinary C ABI int main(int, char **).
NativeHostInput.captureProcess copies the entry argv and selected libc environment;
its argument decoder is shared with explicit-vector capture. The source entry moves
OsHostInput into its owned Execution and provides HostInput to an application Effect
through ordinary source interface specialization. Input lookup before and after
Effect.suspend observes the same captured state. All 42 debug/optimized native lanes
passed across Darwin ARM64, GNU x86-64 and GNU ARM64, with independent C declarations
and actual native/container execution. Reports are .scratch/hosted-start/<target>.json;
logs are entry-input-{darwin,amd64,arm64}.log under .scratch/process-admission.

The suspension case exposed an existing Effect-block typing defect: only explicit
fail statements contributed to inferred failure rows, while run expressions lost
both failure and service rows. MIR then referred to an unlowered open nested runner.
Effect-block inference now unions the symbolic rows of executed computations while
excluding nested deferred bodies. The four-second EffectBlockTyping regression failed
on the missing HostInputError row before the fix and passes afterward. The original
source-entry repro now emits, and all native suspension cases pass. Effect typing,
join, forwarding and suspension regressions passed 16 tests in six files; the earlier
EffectBlockTyping/EffectRuntime/UserInterfaceWitness group passed 14 in three files.

Workspace typecheck passed (18 tasks); formatting passed. Capture fault conformance,
final lint and documentation generation were still running when this entry was written;
inspect entry-input-capture-*, entry-input-lint.log and entry-input-docs.log before
handoff. The standalone repro and MIR dump are marked debug artifacts under .scratch.
The fixture is still not selected by default and has no terminal reporter. Default/
custom/none integration, reporting state/ABI, Wasm entry migration and the remaining
five-ticket audit/full verification are required before completion.

Final entry-input outcomes: all twelve refreshed capture/fault lanes passed; lint,
documentation generation, documentation policy (82 modules), strict OpenSpec validation
and git diff --check passed. All commands from this entry-input slice have finished.
Final whole-repository test/check/release gates remain pending for the complete goal.

## Storage bootstrap calls and cleanup

ExecutionStorageComponent validation now follows executed source calls, Effect runners,
selected callables, source C-export calls and cleanup hooks from each selected operation.
It detects independently driven Execution packages through the same retained result
package set used by native drive emission. The old check only inspected whether the
export's own frame could suspend. A source provider whose create operation called a
helper driving a suspended Execution was incorrectly accepted before this change;
the focused regression now rejects it with SEM0214/DependencyCycle. A Drop-hook
variant is also rejected, while constructing and dropping an unexecuted lazy Effect
is allowed. All 36 Instances tests and workspace typecheck (18 tasks) passed. Final
format/lint logs are storage-bootstrap-{format,lint}.log under .scratch/process-admission.

The exercise also exposed a separate preexisting integration failure: Execution.make
inside an Effect requiring Allocator, with that service supplied by an outer caller,
can fail SEM0142 with unproved service allocation provenance. Keeping the system
allocator local to the construction avoids that unrelated failure and lets the actual
bootstrap test run. The rejected source provider is saved as a debug repro at
.scratch/process-admission/storage-bootstrap-unproved-allocator.silk and must be
resolved or accounted for in the remaining runtime integration audit. Dynamic package
callbacks/cleanup and external supply bootstrap evidence still need their complete
closure audit. This is progress on JUL-129, not completion of it or the five-ticket goal.

## Bounded source report formatter

Added silk/native_report.NativeReport with explicit per-report descriptor, byte and
caller-frame limits. It uses NativeDescriptor for partial writes and EINTR handling,
formats typed failures, fatal traps, callers and retained causes, and stops output
after a failed fragment or exhausted limit. It neither allocates nor observes traps
or payloads. The manifest initially failed its canonical-order check; that ordering
is fixed. Initial source diagnostics exposed incorrect reborrows, module/actor import
selection and unsupported conditional-expression syntax in the fixture; the corrected
fixture has no analysis diagnostics.

All six native debug/optimized formatter lanes passed on Darwin aarch64 and GNU Linux
x86_64/aarch64. Each independently compiled C receiver checks seven exact-byte/status
cases, including partial writes, EINTR, zero progress, committed-prefix failure and
both limits. The object retains no generated runtime symbol. New report-supplies.json
pins the headers; .scratch/native-report contains full supply/execution evidence.
CI runs the probe and uploads that directory; test:native-report is available.
Workspace typecheck passed all 18 tasks and formatting passed. Lint and documentation
checks/generation are recorded in report-format-*.log under .scratch/process-admission;
inspect their terminal results before handoff.

This does not complete JUL-130. Outcome-associated semantic context, cleanup/cause
transport, suspension/reentrancy, source observer wiring, default entry selection and
removal of generated hosted reporting remain required. The earlier bootstrap allocator
provenance repro and remaining JUL-129/134/147 evidence/audit work are also still open.
Final whole-repository test/check/release gates remain pending for the complete goal.

Final formatter-slice outcomes: lint, strict OpenSpec validation, documentation
policy (83 modules), documentation generation and git diff --check passed. All
formatter-slice commands have finished. The full-goal test/check/release gates and
remaining integration tasks above are still pending.

## Real GNU child startup fault admission

The previous formatter turn made concrete progress. This slice closes the known
parent-only fork-sweep gap: a new child-fault receiver executes real forked children
and injects ten startup cases across notice duplication, CLOEXEC, standard streams,
chdir, exec, interrupted/fragmented notice writes, zero progress and partial failure.
Shared observations prove injection and sender progress; parent checks verify the
original descriptor flags and no outstanding child. The exec interception additionally
checks cwd, stream access modes, notice CLOEXEC and closed descriptors above three.
All four debug/optimized GNU lanes passed with the extra pinned receiver headers.

A completely unwritten notice is indistinguishable from successful exec under the
selected EOF protocol; the outcome then carries exit 127. Partial notices are
rejected. OsChildProcess now documents this tested limit. The process conformance
catalog includes the new local evidence; CI and test:native-process-child-faults run
the GNU mode. Detailed reports remain under .scratch/native-process/*-child-faults.json.

Workspace typecheck passed 18 tasks and format check passed. Initial lint rejected
four nested ternaries in the new runner selection; selection was refactored into
an explicit fixture record. The follow-up build/lint/docs checks are logged under
.scratch/process-admission/child-fault-*.log. Full five-ticket integration and final
whole-repository test/check/release gates remain incomplete.

Final child-fault slice results: all four refreshed GNU lanes passed after runner
selection cleanup and the source documentation update. Workspace typecheck (18 tasks),
format check, lint, documentation generation, documentation policy (83 modules),
strict OpenSpec validation and git diff --check passed. All child-fault slice commands
have finished. No full-goal completion claim is made; remaining hosted report context,
entry/Wasm migration, storage lifecycle/bootstrap closure and JUL-147 final gates
remain the active work.

## Failure context survives cleanup

Rechecked JUL-130 and the current NativeTermination/EffectLowering implementation.
The previous child-fault turn was concrete progress. This slice adds a native corpus
regression in which both an unwound Guard and the terminal Primary payload run and
recover Noise during Drop. Before the fix the report named Primary but used noise()
as its origin and omitted main's frame. The initial exact-name test selector matched
no case because Vitest truncates long interpolated names; the shorter selector ran
and failed the actual case. report-cleanup-red.log records that failure.

NativeTermination.preservingFailure now retains site/depth/path and cause fields in
SSA values around the three failure-propagation cleanup paths and terminal payload
cleanup. Returning cleanup restores the active outcome; a fatal trap does not gain
restoration or recovery. This is a semantic fix to the current report ABI, not a
replacement for source-owned report state. report-context-contract.md records the
outcome/recovery/cleanup obligations and the remaining source transport gate.

A first rerun reused a stale shared native artifact because the generated compiler
identity had not been rebuilt. A fresh native cache demonstrated the restored primary
context; the compiler build then refreshed ToolchainIntegrity.generated.ts. The test's
expected provenance coordinates were corrected to the current logical frame anchors.
All six native termination corpus cases passed after that rebuild, including the
combined owner/payload cleanup regression. Workspace typecheck passed 18 tasks and
formatting passed. Lint and strict OpenSpec validation use report-cleanup-*.log under
.scratch/process-admission; inspect terminal results before handoff.

No source observer ABI, state-lifetime migration, default entry or shared Wasm entry
migration is claimed complete. Those and the remaining storage/bootstrap closure,
JUL-147 audit and final full test/check/release gates remain required.

Final cleanup-context slice results: lint, strict OpenSpec validation and git diff
--check passed. All slice commands have finished. The six-case native termination
run and 18-task typecheck above are current; final whole-goal gates remain pending.

## Hosted entry/environment prior-art test comparison

The previous turn fixed and verified cleanup-context semantics. This turn completed
the outstanding targeted test comparison required by JUL-130: Zig entry_point's
main/build pair and Environ map/block tests, plus Rust std/tests/env.rs and the
C-to-staticlib main/library fixture. Both local revisions match the ticket pins;
git diff against HEAD confirms the inspected files are unchanged. Exact content
digests are retained in hosted-prior-art-tests.json.

The comparison corrects overly broad potential inferences: Zig's entry fixture
proves frontend selection/cache differentiation, not a hosted C main lifecycle;
Rust's env.rs does not prove raw snapshots, short buffers or reentrancy. Rust's
staticlib test proves a separate C caller and execution after archive removal,
not absence of runtime globals or cross-target correctness. hosted-inventory.md
now records adoption, deliberate differences and the independent Silk evidence
still needed. No upstream execution is claimed. Formatting, strict OpenSpec
validation and git diff --check passed. This documentation-only slice does not
complete source report transport, default/Wasm entry migration or final goal gates.

## Caller-provided execution allocator provenance

The previous prior-art comparison turn made documentation/admission progress. This
slice resolves the allocator-provenance integration failure recorded under Storage
bootstrap calls and cleanup. A focused ExecutionPackage analysis test reproduced
SEM0142 when work() required Allocator and its caller provided a system allocator
around Effect.catchAll(work(), failed). The ordinary call walk stopped at the
catch combinator: executing its captured protected Effect is not an ordinary call
edge in discovery.calls.

LocalSharedAllocationProvenance now follows specialized Effect parameter identities
from executed Run subjects as well as ordinary call edges. It does not recognize
Effect.catchAll or any provider by library spelling. The focused test accepts the
caller-provided system allocator and rejects a WrongAllocator that ignores the
requested layout. All five ExecutionPackage tests pass. The storage bootstrap test
now uses the original caller-provided composition, removing the local-allocator
workaround; it passes and reaches the intended DependencyCycle diagnostic instead
of failing prematurely with SEM0142.

Workspace typecheck passed 18 tasks. Related provenance/forged-provider tests and
format/lint validation are recorded in allocator-forward-*.log under
.scratch/process-admission; inspect final results before handoff. Temporary debug
logging was removed. The saved storage-bootstrap-unproved-allocator.silk source is
now a historical red repro; its issue is resolved by the tests above. Report-state
transport, entry/Wasm migration, full storage lifecycle/closure audit and final
whole-goal test/check/release gates remain incomplete.

The negative fixture was strengthened to use an explicitly proven shared-value
layout for i32 where an execution package is required, and it asserts the diagnostic
code plus the related source span at that wrong layout expression. This avoids
accepting an unrelated unsupported-origin rejection as proof. All five refreshed
ExecutionPackage tests pass, as do five related provenance/forged-provider tests
and the original bootstrap composition test. The final test edits passed the
18-task workspace typecheck; final format/lint results remain in the named logs.

Final allocator-forwarding slice results: formatting, lint, strict OpenSpec validation
and git diff --check passed. All slice commands have finished. The earlier allocator
provenance blocker is resolved; the remaining full-goal implementation and final
verification requirements above are unchanged.

## Reentrant native storage lifetime evidence

The previous allocator-forwarding turn fixed an implementation defect. This slice
strengthens the existing multiple-package native fixture with an independent C
allocation ledger and a nested invocation sweep. Reentry is injected at its ordinary
register/ready/suspend/complete callbacks. Each nested call must free its own
allocations once and leave every live outer allocation byte-for-byte unchanged;
the outer call must then finish both packages and free all remaining allocations.
The existing normal runner and CI exercise this receiver without another Silk
fixture or a separate compiler pipeline.

An initial receiver invoked reentry from malloc/free. Review against the bootstrap
contract showed that this would make the allocator depend on execution storage,
so that injection was removed before recording admission. The final allocator only
tracks ownership; source callbacks initiate reentry. All 24 refreshed normal native
lanes passed. The six multiple-package lanes each exercised eight callback positions,
in debug/optimized modes on Darwin aarch64 and GNU x86_64/aarch64. Compact exact
receiver/runner/source evidence is in storage-reentry-conformance.json; full supply,
object and execution records are under .scratch/execution-storage.

The storage contract's stale pre-migration Wasm paragraph was reconciled with the
implemented shared frame ABI, while preserving the outstanding entry/export and
bootstrap closure obligations. Documentation explains why allocator-originated
reentry is excluded. Slice validation is recorded under storage-reentry-*.log in
.scratch/process-admission. Borrowed cleanup, the complete bootstrap/supply audit,
source report transport, default/Wasm entry migration and full goal gates remain open.

Final reentrant-storage slice results: all 24 native lanes, workspace typecheck
(18 tasks), format check, lint, strict OpenSpec validation and git diff --check
passed. All slice commands have finished. Final whole-goal test/check/release gates
and the remaining implementation obligations are still pending.

## Structured-exit storage accounting

The previous turn added verified callback reentry. This slice applies the same
allocation ledger to the existing latched-destruction fixture and a suspended
failure variant of the multiple-package fixture. The variant parks, fails with a
typed Failure, recovers outside that suspended body, and completes with the other
package. The runner derives it from the shared source and rejects a missing body
anchor. Each lane retains its exact generated source.

All 30 normal native lanes passed on Darwin aarch64 and GNU x86_64/aarch64 in
both optimization modes. The 18 ledger/reentry lanes cover normal completion,
cancellation of a parked package and typed failure after parking. All allocations
must be released once; nested callbacks must preserve live outer storage bytes.
storage-reentry-conformance.json now records all three lifecycle cases and exact
source/receiver/runner digests. Full target/supply/assembly evidence remains under
.scratch/execution-storage. The required native CI runner includes these cases.

Validation logs use storage-exits-* under .scratch/process-admission. This strengthens
JUL-129 structured-exit evidence but does not complete borrowed-cleanup/bootstrap
closure, source report transport, hosted/Wasm entry migration or JUL-147 final gates.

Final structured-exit slice results: all 30 native lanes, workspace typecheck
(18 tasks), format check, lint, strict OpenSpec validation and git diff --check
passed. All slice commands have finished; the full goal remains active and its
remaining implementation and final test/check/release requirements are unchanged.

## Independent Wasm lifecycle accounting

The previous native structured-exit turn supplied verified lifecycle evidence.
This slice extends the independent Wasm C probe to use the same counting/reentry
receiver for latched cancellation, normal multiple-package completion and typed
failure after suspension. A small freestanding C memory boundary supplies byte
operations; the receiver supplies malloc/free. These probes therefore verify
actual release calls without depending on the default Wasm allocator's no-op free.
They link without undefined symbols or host imports and use ordinary source C
exports plus the selected execution-storage component.

All eight independent C probes and twelve existing Driver lifecycle/refusal lanes
passed in the first run. The default Wasm entry and bare traps were not changed.
LifecycleFixture.mjs now owns the checked source transformation shared by native
and Wasm runners; an exact digest comparison verified that all four native lifecycle
sources remain byte-identical to the previously executed sources. The final runner
also retains generated .silk files beside bitcode, IR, C and Wasm artifacts.

Validation and the final rerun are logged under storage-wasm-count-* in
.scratch/process-admission. This does not close borrowed-cleanup/bootstrap supply
coverage, source reporting state, hosted/Wasm entry migration or the final goal gates.

Final Wasm accounting slice results: eight independent C probes and twelve Driver
lifecycle/trap lanes passed, all with zero imports. storage-wasm-conformance.json
records exact source, receiver, artifact and shared-generator identities plus the
observed exports/outcomes. Generated .silk evidence was retained by the final run.
Workspace typecheck (18 tasks), format check, lint, strict OpenSpec validation and
git diff --check passed. All slice commands have finished. Full-goal implementation
and final test/check/release gates remain incomplete.

## Lexical report transport decision

The reporting semantic contract now has a selected replacement design in
report-observer-contract.md. It assigns observation to an owned lexical Effect
boundary, optional persistent context storage to source, and per-call/per-outcome
transport to the compiler. The protected boundary handles its complete failure
row before releasing observer state. Recovery cause lifetime covers precisely
handler application and execution, including suspension. Ordinary C reentry starts
without borrowing an existing observer, and callback execution disables recursive
observation without a process-global guard.

The protocol specifies seven semantic events, explicit handle ownership and
artifact-lifetime text. Primary identity/origin survive optional context storage
exhaustion. Source owns a bounded pool and allocation-free output fallback; emitted
semantic metadata and the irreducible trap remain compiler responsibilities.

This turn changes planning/specification only. The observer intrinsic, source pool,
internal context transport, default entry and old-path deletion are still required;
no ABI implementation or ticket completion is claimed. The previous NativeReport
formatter and cleanup regression remain the only implemented reporting pieces.

## Bounded source context pool

The next turn added an executable source pool fixture under conformance/native-report.
It owns one bounded reservation through ExecutionStorage, retains immutable semantic
strings and implements persistent nodes with reference counts. Release uses an
intrusive pending list in dead nodes, so it needs no allocation or recursive calls.
The fixture covers capacity refusal, original metadata, retained primary/frame/cause
sharing, two links to the same child and 100 complete release/reuse cycles.

The independent C receiver counts the two acquisition sites, injects failure at
each, and requires no live allocations or invalid frees on every exit. All six
context lanes and the six existing formatter lanes passed on Darwin ARM64 and both
GNU Linux targets in debug and optimized modes. Logs are report-context-{darwin,
amd64,arm64}.log under .scratch/process-admission; full supply reports are under
.scratch/native-report. The source owner remains a fixture pending integration;
neither observer dispatch nor NativeTermination global-state removal is claimed.

The initial compile exposed inferred borrowed string lifetimes and a pointer
qualifier-conversion mismatch. Explicit artifact-lifetime strings and raw-memory
access helpers resolved them. The selected protocol now spells its semantic text
lanes as string<'static>, grounded in that successful source compilation.

report-context-conformance.json retains the six executed pool lanes with exact
source/receiver/runner digests. Workspace typecheck (18 tasks), format check, lint,
strict OpenSpec validation and git diff --check passed. All commands from this
slice have finished. Full-goal implementation and final test/check/release gates
remain outstanding.

## Source context rendering

The pool fixture now drives NativeReport with retained primary/frame/cause nodes.
It writes the primary and outward frames before causes and their frames. A missing
primary context uses the separately supplied semantic identity/origin; a missing
cause emits truncation without inventing a cause identity. NativeReport.truncate
is a new source operation shared with frame-limit exhaustion. It writes at most
one marker within the byte budget and suppresses every later write.

The C receiver verifies exact output for normal traversal, absent primary context,
frame limit, byte limit, partial output followed by EIO, and absent cause context.
It also verifies repeated truncation is inert and that all six modes use exactly
the two construction allocations and free both. Traversal is heap-allocation-free;
its recursion is bounded by pool capacity. Release remains iterative.

All six native context/rendering lanes and six formatter lanes passed on Darwin
ARM64 and both GNU Linux targets, debug and optimized. Updated exact evidence is in
report-context-conformance.json; logs are report-render-* under .scratch/process-admission.
The compiler build and generated documentation completed. The context owner is still
a fixture, and observer/compiler transport and entry migration remain unfinished.

Final rendering-slice checks passed: workspace typecheck (18 tasks), format check,
lint, documentation policy (83 modules), strict OpenSpec validation and git diff
--check. Generated documentation includes NativeReport.truncate. All commands
started by this slice have finished. The full goal's final test/check/release gates
have not been claimed or satisfied by these targeted results.

## Observer frontend contract

Intrinsic.observeDiagnostics now has a builtin identity and frontend type contract:
owned state S, represented callback F and an infallible once Effect<A ? R> produce
an infallible once Effect<A ? R>. A source generic wrapper passes explicit primitive
arguments <S, A, R, F>; ordinary generic calls infer the public wrapper's arguments.
The callback is an ordinary static-lifetime function returning usize with a
NonParking representation bound and a universally quantified mutable state borrow.
It may run the formatter internally, avoiding an additional lazy callback Effect
whose execution would need a separate closure proof.

The accepted intrinsic catalog fixture and a targeted row/lifetime assertion pass.
A protected Effect with an escaping typed failure is rejected by generic inference
(SEM0052). The generated intrinsic inventory was refreshed from the built catalog.
This is frontend admission work only: runtime lowering, complete executed callback
validation, observer-owned continuation fields and global report transport deletion
remain required. No executable observer behavior is claimed by these tests.

Final frontend-slice validation passed: all 18 IntrinsicCatalog tests, workspace
typecheck (18 tasks), format check, lint, strict OpenSpec validation and git diff
--check. All commands from this slice have finished. Full-goal runtime integration
and final test/check/release gates remain outstanding.

## Direct observer callback admission and cleanup summaries

DiagnosticObservation now checks specialized callback execution before lowering.
NonParking rejects external parking but permits nested transfers, so it cannot by
itself satisfy the observer contract. SEM0216 rejects either mode or an unavailable
callback summary and points at the incoming observation call. The common path for
programs without observation returns immediately from the retained intrinsic list.

A destructor regression exposed missing cleanup dependencies in suspension summaries.
ExecutableOrigin now associates explicit drops, structured-exit releases and
replacement cleanup with their execution regions. Instances rebuilds the final
graph after ownership facts are available. Deferred Effect cleanup remains attached
to its deferred region, so construction alone does not execute it. Execution edges
include actual hooks rather than extra declarations retained for witness evidence.

The regression covers direct callbacks, executed nested transfer, lazy construction,
implicit and explicit local cleanup, cleanup through an ordinary callee, unexecuted
versus executed Effect cleanup, and replacement of an observer-state field. This
does not implement runtime observation, recursive-observer admission or entry
migration; those remain required.

Final closure-admission checks passed: 42 tests across IntrinsicCatalog, Diagnostic,
SuspensionMir and SuspensionOwnership; workspace typecheck (18 tasks), format
check, lint, strict OpenSpec validation and git diff --check. Generated diagnostic
documentation includes SEM0216. All commands from this slice have finished. The
goal remains incomplete, including runtime observer transport and final whole-goal
test/check/release verification.

## Observer capture identity verification

An intrinsic-catalog regression now realizes two observation constructions through
the generic source wrapper, with an affine state and two different protected
bodies. Each environment retains state, callback and body in parameter order with
Take access. The callback remains an exact represented callable, while the body
retains its hidden Effect identity; the two body identities remain distinct.

This establishes that existing environment discovery can carry the three inputs
needed by runtime scope lowering. It does not prove invocation, cancellation or
destruction of an observed runtime scope. No backend observer operation was added
in this turn, and runtime transport remains the next implementation gap.

The capture regression, workspace typecheck (18 tasks), format check, lint, strict
OpenSpec validation and git diff --check passed. All commands from this turn have
finished; full-goal implementation and final test/check/release gates remain open.

## Explicit MIR diagnostic observation scope

The running observation primitive now emits DiagnosticScope with owned state and
callback locals, separate cleanup plans, nested protected execution and result
shape. It evaluates all three arguments before entering the scope. MIR traversal,
encoding, normalization, cleanup transformations and suspension ownership now
visit the body; scope state and callback are included in nested liveness.

The capture regression verifies both protected bodies lower to scopes, retain the
state's Drop hook and do not also emit an ordinary drop for the same state local.
This is structural evidence only. DiagnosticScope is deliberately excluded from
accepted linear operations until backend scope expansion exists, preventing the
dispatch switch from silently ignoring an unimplemented operation.

All 34 tests across IntrinsicCatalog, StoredEffectMir, SuspensionMir and
SuspensionOwnership passed. Workspace typecheck passed (18 tasks), followed by a
compiler typecheck after the linear-operation exclusion. Format and strict
OpenSpec checks passed. Runtime callback dispatch, suspended scope cancellation,
per-call report transport and default entry migration remain required; no ticket
completion is claimed by this MIR slice.

Lint initially rejected a nested ternary in the new verifier branch. Explicit
branches fixed it; the final workspace typecheck (18 tasks), format check, lint,
strict OpenSpec validation and git diff --check all passed. All slice commands
have finished. Full-goal test/check/release gates remain outstanding.

## Diagnostic scope control expansion and retained owners

MIR linearization now expands the protected execution between explicit observer
enter and leave operations. Normal completion moves the result, leaves the scope,
then drops callback and state in reverse acquisition order. Trap paths retain
their terminal control. Native root discovery gives state stable mutable address
storage for callback borrows. The native dispatcher explicitly rejects the two
boundary operations until context transport exists, so this is not executable
observer admission.

The capture regression follows control targets from function entry and verifies
that the protected invocation runs between the boundaries, followed by owner
cleanup. A second source program suspends under observation with an affine state
and a callback carrying a captured word. Its relay retains the state with its Drop
hook and retains the represented callback. Success restores both, while the
failure plan releases the state. Runtime observer-pointer restoration and actual
cancellation remain unproven and still require implementation.

All 35 tests across IntrinsicCatalog, StoredEffectMir, SuspensionMir and
SuspensionOwnership passed. Workspace typecheck (18 tasks), format check, lint and
strict OpenSpec validation passed. Native observer dispatch, per-call diagnostic
context transport, entry migration and final full-goal gates remain outstanding.

## Observer callback recursive-entry admission

ExecutableOrigin now records observation entry nodes separately from suspension
modes. Reverse execution reachability publishes the function bodies that can enter
observation; DiagnosticObservation rejects such callback closures with SEM0216.
The traversal handles call cycles and shares the actual cleanup dependencies.
Programs without observation roots skip the reverse traversal.

The first lazy-construction regression exposed an eager graph edge from the
observation constructor. Construction now only registers its deferred execution;
running the returned Effect connects the caller to it. A captured-owner regression
then exposed missing cleanup edges for unstarted Effects. The graph now follows
owned Effect captures at explicit drops and binding/parameter exit cleanup,
without executing the protected body. Composite cleanup alternatives also expose
their actual hooks.

The regression covers direct entry, a callee, a call cycle, local destructor entry,
explicit and implicit release of an unstarted Effect with owned state, parameter
cleanup, harmless lazy construction, an unexecuted lazy body and executed body
cleanup. Separate source wrappers keep the captured-owner case focused on cleanup
rather than the existing polymorphic-recursion rule. All 65 tests across
IntrinsicCatalog, Instances, SuspensionMir and SuspensionOwnership passed.

Native ABI inspection confirms that observer transport must cover ordinary calls,
C export thunks, suspension steps and resume/driver thunks. No hidden observer ABI
was installed in this turn. Complete indirect/composite cleanup coverage and
native transport remain required before executable observer admission.

Workspace typecheck (18 tasks), formatting, lint, strict OpenSpec validation and
git diff --check passed. All commands from this slice have finished. The goal and
its full test/check/release gates remain open.

## Initial native observer argument transport

Native declarations now add a hidden observer pointer when the module retains a
diagnostic scope. It follows source parameter lanes and precedes suspension lanes.
Each ordinary function initializes an invocation-local observer slot. Ordinary,
synchronous and initial suspension-aware calls load and forward that reference;
C export thunks keep their external signature and supply null. Naked machine
functions retain their physical signature. Native pointer-type retention includes
diagnostic scopes.

The capture regression now declares native functions and checks the callback's
eight source lanes plus observer pointer. It renders an ordinary C export thunk
and checks its argument-free C signature and null observer argument to the private
implementation. The existing native iterative coroutine-protocol test passed.
These are declaration and existing-path checks, not executable observed reporting.
Scope callback dispatch and child/resume observer restoration remain unimplemented,
as does per-outcome diagnostic ownership. Scope emission still fails explicitly.

All 29 tests across IntrinsicCatalog, SuspensionMir and SuspensionOwnership passed,
along with the native coroutine-protocol regression, workspace typecheck (18 tasks),
formatting and lint. Full-goal implementation and test/check/release gates remain
outstanding.

## Synchronous native observation and fatal dispatch

NativeDiagnosticScope now preallocates synchronous descriptors and represented
callback capture storage at function entry. Enter selects the descriptor; leave
restores its predecessor before ordinary owner cleanup. NativeCallable centralizes
capture argument restoration for both ordinary invocation and diagnostic adapters.
The adapter invokes the source callback with a null observer, so callback traps
remain bare. Suspendable scope descriptors still require persistent frame storage.

Fatal sites now dispatch event 6 with immutable reason/origin strings through the
current observer and then emit the machine trap. Null observation does not invoke
the callback. The first native emission exposed the LLVM wrapper's integer-only
comparison API; the presence check now converts the pointer to the target word
before comparing with zero. Typed failure/cause contexts are not transported yet.

The independent observer fixture passed debug and optimized execution on Darwin
ARM64 and GNU x86-64/ARM64. It verifies captured callback arguments, normal Drop,
fatal source output, callback traps, C reentry, disabled observation, nested scopes
and output refusal. The parent requires trap-signal termination and checks pipe
output; no generated runtime dependency is retained by the source object. Signal
and wait header pins were added. report-observer-conformance.json records the six
executed lanes, and the context-pool evidence was refreshed from the same final run.
All six formatting lanes and all six context lanes also passed.

Observed programs using the old generated entry are explicitly rejected until
source entry migration supplies the hidden ABI. Child/resume observer restoration,
failure-context ownership and default source reporter composition remain required.
No full observer or ticket completion is claimed.

All 32 tests across IntrinsicCatalog, StoredCallableMir, SuspensionMir and
SuspensionOwnership passed, plus the existing native coroutine-protocol regression.
Workspace typecheck (18 tasks), formatting and lint passed. Full-goal test/check/
release gates remain outstanding.

Final evidence formatting, strict OpenSpec validation and git diff --check passed.
All commands from this slice have finished; the full goal remains active.

## Observer relay frame transport

MIR now marks an Observer header word after Parent and State in modules retaining
diagnostic observation. Planning and verification agree on the extra word and
shift retained payloads past it. Native declaration uses the same module predicate.
NativeCall retains the current observer in that frame word; native resume adapters
load it between source arguments and the transfer/frame/path arguments. The existing
observer suspension test now verifies the header and payload separation, while
ordinary coroutine tests continue to require the two-word unobserved header.

This does not yet admit suspended observation: scope descriptors still require
persistent storage, child transfers still lack their observer lane, and cancellation
cleanup transport remains pending. No runtime claim is made for those paths.

Workspace typecheck (18 tasks), formatting and lint passed. All 26 tests in
IntrinsicCatalog and CoroutineFrame passed using the compiler package configuration.
The initial root-level invocation finished with two default five-second timeout
failures; the corrected package invocation passed without changing test timeouts.
Formatting of the updated contracts and git diff --check passed. Full-goal gates
remain outstanding, and all commands from this slice have finished.

## Observer child transfer transport

ContinuationTransfer now defines a six-word private header. The added observer word
follows the existing child, head, append, execution-owner and storage-owner words.
NativeProgram allocation, ExecutionPackage's initial segment and its initialization
loop share the header size. Suspension origins store their current observer (or null
for unobserved code); child adapters load it after source arguments and before the
private suspension arguments. This complements the previous relay save/resume load.

All five ExecutionPackage tests and the selected native private-iterative suspension
regression passed. Workspace typecheck (18 tasks), formatting and lint passed.
These checks establish the revised allocation and ordinary suspension path; they do
not prove suspended observer lifetime. NativeDiagnosticScope still rejects that
path until persistent descriptors and cancellation ownership are implemented.
Full-goal conformance evidence and test/check/release gates remain outstanding.

## Diagnostic callback owner storage

NativeDiagnosticScope no longer copies represented callback captures into a second
tuple. Both state and callback are address roots. Descriptors borrow the callback
owner's storage, and the adapter reads each capture using its concrete address
layout. This prepares persistent descriptors to point at the retained callback
owner without introducing another independently managed capture buffer.

The two focused observer retention tests passed, including address-root assertions
for both owners. All 18 formatting, synchronous observation and context fixtures
passed across Darwin ARM64 and GNU x86-64/ARM64, debug and optimized. Observer and
context evidence were refreshed from those executions. Workspace typecheck,
formatting and lint passed; git diff --check passed. Suspended scope descriptors
remain explicitly rejected pending persistent storage and cancellation cleanup.

## Persistent observer descriptor implementation and failing runtime probe

CoroutineFrame now reserves four pointer words per lexical descriptor after every
mutually exclusive payload. MIR records descriptor identities and offsets, encoding
includes them, and verification rejects overlap or noncanonical allocation size.
NativeFunction binds descriptors to the acquired/reused invocation frame. State and
callback owners retain their existing frame-backed address roots. The previous
suspendable-descriptor rejection has been removed; runtime admission is unproven.

Execution body calls and generated release helpers now carry the observer argument.
Compiler storage bootstrap calls pass a null observer through NativeExecutionStorage
to avoid a storage-dependent reporting cycle. The new observer-suspended fixture
uses a synchronous C entry with Execution.make/drive and the source storage component.
Its receiver separates initial result, Drop count and event count failures.

Nine focused MIR/catalog/frame tests passed, including rejection of an overlapping
descriptor. The ordinary native private-iterative regression passed. Workspace
typecheck and formatting passed; lint passed after removing two unused bindings.
However, the Darwin debug suspended fixture returns 1 instead of 42 on its first
normal invocation. It compiles and links, but runtime behavior is incorrect. The
runner now saves emitted LLVM IR beside fixture sources for diagnosis on the next
run. The failed run is terminal; no conformance success is claimed. Existing compact
report evidence predates this slice and must be refreshed after correction.
Cancellation restoration and full-goal gates remain outstanding.

## Resumed observation and cancellation native admission

The result-1 failure came from nonterminal direct Intrinsic.suspendEffect: its
origin transfer omits the enclosing continuation, making the division unreachable.
The public Effect.suspend boundary retains that continuation. The reporting fixture
now uses the public operation; direct-suspend-regression.silk preserves the separate
defect and task 5.6 explicitly requires its repair. It has not been dismissed as a
fixture issue or marked fixed.

Cancellation now loads the observer retained by each frame, restores a descriptor's
predecessor before destroying its callback/state owner, and restores the caller's
observer after frame cleanup. The independent cancellation receiver verifies one
guard and state destruction, guard-destructor reporting, bare state-destructor traps
after leaving observation, and an inner state-destructor trap reaching the outer
observer. All fatal cases require machine-trap signals and checked pipe output.

All 30 current formatting, synchronous observer, suspended observer, cancellation
and context lanes passed across Darwin ARM64 and GNU x86-64/ARM64, debug/optimized.
Ten selected catalog/frame/native regression tests passed. Workspace typecheck
(18 tasks), formatting and lint passed. Four compact report evidence files were
refreshed from exact current source digests and execution results, followed by
formatting and git diff --check. All commands are terminal. Owned failure context
transport, the direct-intrinsic defect, default startup/report integration and
full-goal audit/check/release gates remain outstanding.

## Direct suspension continuation repaired

Direct suspension now materializes the primitive's existing generated Effect runner.
Only that runner emits the terminal transfer origin; the enclosing caller receives
ordinary complete/relay control and retains its following computation. Provisional
planning includes directly run suspension builtin runners, which were previously
excluded along with inline builtins. No library-name recognition or wrapper shim
was introduced.

The initial planning edit exposed OWN0020 because that exclusion omitted the new
runner's control authority. Correcting it restored all 29 existing suspension,
ownership and intrinsic tests. The new structural regression proves a retained
caller relay and a separate primitive origin. The standalone reproducer now runs
in the shared native corpus and returns 42. The suspended report fixture crosses
both public and direct suspension boundaries before dividing, and all 30 report
lanes passed on the three native targets in debug and optimized modes.

Task 5.6 is complete. Current source digests and native evidence were refreshed.
Final workspace typecheck (18 tasks), formatting, lint and git diff --check passed.
All commands from this slice are terminal. Typed-failure context ownership,
startup/report integration, migration audit and the full-goal gates remain open.

## Source diagnostic event protocol

The bounded Context candidate now supplies an ordinary ObserverState callback for
all seven diagnostic events. Produce/Propagate/WithCause create persistent nodes;
Retain/Release account for handles; Unhandled returns policy 1 independently of
output success; Fatal formats the trap and any retained cause and returns 0.
The renderer borrows a pool address while the owning state remains live, because
the initial simultaneous pool/writer field borrows failed MIR loan verification.

Rendering conformance now drives the callback protocol, verifies every Release
result is zero, retains and relinquishes a shared terminal context, and adds fatal
output with and without causes. The same callback is admitted through the intrinsic
around a normal protected Effect. The independent receiver checks exact bytes,
both allocation refusal ordinals, the fixed two-allocation bound and full release.
All 30 current native lanes passed on Darwin ARM64 and GNU x86-64/ARM64 in both
build modes. Exact evidence was refreshed after verifying source digest matches.

Workspace typecheck passed (18 tasks), and git diff --check passed. This slice
implements and verifies the source adapter only: native typed-failure lowering
still does not emit events 0–5 or transfer their owned contexts. Default source
reporter installation, startup integration and full-goal gates remain outstanding.

Final formatting and lint passed. All commands from this slice are terminal.

## Native owned failure value primitives

NativeDiagnosticContext now supports dispatch to an explicitly retained observer;
ordinary lexical dispatch delegates to it. NativeDiagnosticFailure provides a
six-field metadata value: observer, handle, identity pointer/length and origin
pointer/length. Produce, Retain, Propagate, WithCause, Release and Unhandled preserve
the originating observer and immutable fallback metadata. WithCause guards against
mixing handles from different owners. Pack/unpack transfer metadata without adding
a reference. These operations are not yet wired into native Effect outcomes.

A 32/64-bit LLVM structural regression changes lexical selection after production
and verifies that later operations do not reload that selection, preserve fallback
metadata and serialize the expected fields. It passed after correcting the helper's
Effect generator and pinning the public LLVM type annotation. Workspace typecheck
passed (18 tasks). All 30 native reporting lanes passed after the dispatch refactor,
and their evidence was refreshed. Formatting and git diff --check passed. Lint found
one unused binding in the new emitter, which was removed without changing emitted
runtime behavior. Failure-return/catch/cleanup integration and old-global removal
remain open, along with startup, migration audit and full-goal gates.

The corrected lint run passed. All commands from this slice are terminal.

## Native completion boundary and recursive result discovery

NativeReturn.complete now owns ordinary MIR completion, selective/direct/composite/
stored-Effect propagation and allocation failure returns. It validates the declared
semantic lane count, selects synchronous or suspension completion, and preserves
frame release through NativeSuspension.returnStep. The latter returns its LLVM
instruction so completed suspended returns can also receive source debug locations.
No failure metadata has been added to the result ABI. The hosted inventory now
records the remaining driver, thunk, execution, callback and foreign boundaries.

The first focused run passed 37 tests but exposed a stack overflow in the existing
recursive-aggregate-return native corpus case. It occurred in origin discovery,
before native emission: compositeEffectRepresentationOf traced arbitrary result
expressions and had no recursion guard. It now rejects non-Effect contracts and
bounds recursive queries by specialized instance and expression identity. The
existing discovery recursion test includes the aggregate case. That test, the
existing native corpus case, and all 41 discovery/composition tests passed. This
was discovered in the accumulated migration work; the full repository baseline
was not separately tested. Task 5.7 records the correction.

Workspace typecheck passed (18 tasks), formatting and lint passed, and git diff
--check passed. The initial 30 reporting lanes passed after return consolidation;
a fresh run after the discovery fix is in progress. Full typed-failure transport,
source default entry/report installation, global removal, migration audit and the
full-goal checks remain outstanding.

The fresh 30-lane report run and source application-result witness test passed.
Compact report evidence was refreshed after checking source digests and statuses.
All commands from this slice are terminal. The initially failing aggregate case
now passes; no ticket-level completion is claimed by these focused checks.

## Selected handler scopes for owned diagnostic transport

MIR Execution now identifies the original caught outcome for a selected recovery
handler. Lowering encloses both callable application and returned-Effect execution;
protected success and unselected propagation remain outside. Normalization keeps
the association, and canonical MIR encoding includes it. Control expansion records
ordered recovery outcomes and an explicit observation boundary on each block.
Observation owner cleanup now has a separate block after leaving the fresh body,
so it returns to the enclosing recovery context before dropping callback/state.

Verification checks the caught producer and its conditional or matched selection.
The structural regression proves both selected applications/runs, the unselected
propagation and the joined return, and rejects invalid or success-branch scope
annotations. The existing observer regression checks the fresh body boundary and
its absence on owner cleanup. These annotations do not yet own runtime handles:
native failure/cause values, their call ABI, continuation retention and release
remain pending, as does removal of the legacy caught globals.

The initial scope run passed 25 catch/runtime/suspension tests, six focused observer
tests and workspace typecheck (18 tasks). All 30 native report lanes passed after
the cleanup-block split, and the shared native primary-preserving cleanup case
passed. Verification was then tightened to check the selected branch; the focused
regression passed, and its broader checks are in progress. Full-goal completion
and the remaining startup/report/migration gates remain open.

The tightened selection verifier passed all 25 catch/runtime/suspension tests and
workspace typecheck (18 tasks). Formatting, lint and git diff --check passed.
Compact native reporting evidence was refreshed and formatted. All processes from
this slice are terminal. No runtime context ownership or ticket completion is
inferred from these structural boundaries; the full requested goal remains active.

## Independent execution diagnostic lifetime correction

Before extending the selected-cause ABI, inspection found that all three initial
Execution body call paths forwarded the driving invocation's observer pointer.
A parked, detached Execution can outlive that lexical observer. The cancellation
fixture now stores an Execution outside the observed drive, destroys the observer
owner, and then cancels the retained body. Its C receiver checks owner destruction
before guard cleanup and rejects any call to that expired observer. The pre-fix
Darwin debug run failed with receiver status 25 on this escaped-cancellation case
(`observer-escape-before.log`). The exact unexpected child signal was not recorded.

NativeCall.argumentsFor now has an explicit Independent observation boundary, used
by direct initial bodies and both initial coroutine-package body paths. They pass
null rather than borrowing the driver's observer. Ordinary calls and drive outcome
callbacks still inherit the caller's observation. Observations installed inside an
Execution body remain owned by its continuation frames. This also gives the future
selected-cause ABI an explicit boundary at which to clear incoming borrowed causes.
No cause ABI or owned failure result was added in this slice.

The fixture also checks an unobserved synchronous body trap and a completion
callback trap reported through the driving observer. All 30 reporting lanes passed
on Darwin ARM64 and GNU Linux x86-64/ARM64 in debug and optimized modes. The six
focused observer tests passed. Workspace typecheck passed (18 tasks), source stdlib
content was regenerated, and documentation generation completed; the generated
Execution.drive reference contains the new ownership contract. The OpenSpec
scenario, observer contract, conformance README and exact evidence were updated.
Task 5.8 records the fixed lifetime defect. Full runtime failure transport, source
startup/report integration, migration audit and full-goal gates remain outstanding.

Formatting, lint and git diff --check passed. All processes from this slice are
terminal. The selected-cause call ABI remains the next transport work; the unsafe
observer inheritance prerequisite is now fixed and verified.

## Storage bootstrap observation closure

The bootstrap audit found an omitted execution edge in DiagnosticScope: provider
state and callback cleanup were not traversed. A provider whose observation-state
Drop drives an Execution was accepted before the fix; the regression failed with
Available MIR where DependencyCycle was required. The walk now follows both cleanup
plans and the observer target. Callback resolution uses concrete environment/stored
realization arguments, matching native emission. A harmless observation remains
accepted, and separate state-destructor and observer-callback storage recursion
cases are rejected with SEM0214/DependencyCycle before object emission.

The focused regression passed after the fix, all 36 instance tests passed, and the
additional callback case passed in the focused provider test. Workspace typecheck
passed (18 tasks). The current RuntimeComponent binding, signature admission,
source-owned state/configuration, demand-only retention and capability diagnostics
were inspected; task 2.1 is now complete. The storage inventory was rewritten to
record the actual four-operation ABI and six-word transfer/package header rather
than the superseded generated-C baseline. External supply/bootstrap closure,
borrowed/dependent cleanup and final native/Wasm audit gates remain in tasks 2.4/2.5.

`openspec validate complete-native-runtime-migration --strict` passed. This is
structural planning validation, not evidence that the remaining admission inventory
or all required conformance lanes are complete; task 1.4 remains open. Full
failure-context transport, startup/report integration, migration closure and full
repository gates also remain outstanding.

## Borrowed cause invocation and continuation ABI

Internal observed functions now receive a six-field by-value borrowed cause after
the observer argument and before suspension-control lanes. Ordinary calls forward
the current aggregate; independent Execution roots, C exports, observer callbacks
and storage bootstrap pass empty observation/cause inputs. C signatures remain
unchanged. A seeded LLVM ABI regression at 32 and 64 bits proves forwarding and
independent-root clearing without relying on source fixtures whose causes are
currently empty.

Observed relay frames retain both the incoming invocation cause (for resume) and
the current cause (for cancellation), separately from the observer. Their header
has fifteen words. The shared transfer/package header has twelve words, including
six current-cause fields; child adapters load those fields. Observation descriptors
reserve ten words and restore their previous cause before owner cleanup. These are
borrowed copies, not additional owned pool references. Linear blocks currently
restore incoming cause or clear it inside a fresh observation boundary; selected
recovery-outcome lookup is still pending.

Workspace typecheck passed (18 tasks), 43 intrinsic/suspension/selective-catch tests
passed, the additional seeded ABI test passed, and 14 continuation/package tests
passed. All 30 native report lanes passed across Darwin and both GNU targets, and
all 20 Wasm execution-storage lanes passed after changing the shared transfer
layout. Report conformance evidence and active layout inventories were refreshed.
Formatting and git diff --check passed. Logs use .scratch/diagnostic-cause-*;
native runs retain the observer-dispatch target logs.

This completes a transport prerequisite, not JUL-130 or the overall goal. Owned
outcome metadata, selected-handler cause lookup, automatic events 0–5, terminal
observation, source startup integration, remaining admission audits and full
repository/release gates remain outstanding. No additional task was marked done.

## Fatal dispatch consumes the borrowed cause ABI

NativeTermination now dispatches fatal events through NativeDiagnosticContext.fatal.
The emitter loads the selected observer once, reads the active borrowed cause,
and forwards its handle only when its originating observer matches. A foreign
pool handle becomes zero; no callback receives another observer's handle. Fatal
reporting neither retains nor releases the borrowed reference, and termination
still emits the machine trap after dispatch.

A seeded 32/64-bit LLVM regression verifies the owner comparison, guarded handle,
one dispatch call and one observer load. All eight selected observer/metadata
regressions passed, workspace typecheck passed (18 tasks), and all 30 native report
lanes passed. The report conformance evidence was refreshed. These native fixtures
still have no automatically selected failure cause: the nonzero-input proof is
structural ABI evidence, not an end-to-end source recovery test. Logs are recorded
under .scratch/fatal-cause-* and the existing observer-dispatch target logs.

The ownership investigation confirmed that recoveryOutcomes currently annotate
selection but do not allocate owned diagnostic storage. Keeping the original
EffectOutcome payload alive would be incorrect after a handler consumes it. The
remaining implementation must transport metadata independently of source payload
layout, retain that metadata through suspension, and release it at selected-handler,
propagation, cancellation and terminal ownership boundaries. Fatal cause dispatch
is a prerequisite; automatic events 0–5 and owned failure transport are unfinished.
No ticket or additional OpenSpec task is marked complete by this slice.

## Shared private result packing

NativeResult now represents source lanes separately from a private diagnostic
aggregate and validates metadata presence and lane count before packing. Its
unpacker distinguishes synchronous scalar/aggregate results from suspension-step
results with a leading status. NativeCall, NativeCallOperation, NativeReturn and
child/resume/driver paths in NativeSuspension now use these shared operations.
Production declarations still select diagnosticResult: false: this refactor does
not yet enable an owned diagnostic return field or silently invent an empty one.

The seeded 32/64-bit LLVM test forwards nonempty metadata through synchronous and
suspension-step results and verifies the separate field indices. Missing metadata,
extra metadata and incorrect source lane counts are rejected. All 55 focused
intrinsic/catch/suspension/frame/package tests passed, and the strengthened seeded
step test also passed. All 30 native report lanes and all 20 Wasm storage lanes
passed. Report evidence was refreshed after the result-unpacking names changed.
Workspace typecheck, formatting and lint passed; logs use .scratch/private-result-*.

Next, declaration shapes and call-site result consumers must carry the diagnostic
field together, with per-outcome storage and explicit ownership transitions. Merely
adding a return trailer while discarding it in the existing array-only consumers
would lose ownership, so that production activation remains outstanding. Automatic
failure events, selected-handler cause ownership, terminal observation, source
startup, migration audits and the full repository/release gates are still open.
No ticket or additional OpenSpec task is marked complete.

## Independent outcome storage and cleanup

Potentially failing internal outcomes now receive six-word metadata slots separate
from their payload. NativeFunction allocates and initializes synchronous slots;
CoroutineFrame reserves persistent slots after the observation descriptors.
Acquisition initializes only a new frame, while resume binds its existing slots.
MIR verification checks canonical offsets, ordering and bounds, and MIR text now
includes diagnostic-outcome fields. Recovery blocks borrow the slot named by
recoveryOutcomes instead of always reverting to the incoming cause.

NativeDiagnosticOutcome provides borrowing, taking, replacement and release.
Taking clears ownership; replacement publishes the new reference before invoking
the previous owner's release callback. Function completion releases remaining
slots. Scope exit releases slots matching its observer before state destruction;
cancellation releases those matching slots before the scope's first owner cleanup
and clears remaining slots before releasing the frame. Borrowed incoming causes
are not independently released by these slot operations.

The seeded 32/64-bit LLVM ownership test proves clear/publish/release ordering and
observer matching. A revised suspension regression proves separate frame slots
and rejects an overlapping layout. The first attempted fixture put recovery in a
separate catchAll runner that had no frame descriptor; the final case performs
Intrinsic.catchFailure in the same invocation as a later suspension. The missing
frame on a suspendable catchAll runner remains an investigation target before
claiming selected-handler suspension admission; it was not proven a runtime bug
or resolved by changing this fixture.

All 56 focused tests passed, followed by the two strengthened ownership/layout
tests. All 30 native report lanes passed after adding a recovered input before the
suspended fixture's two suspension boundaries. This exercises frame allocation,
resume and empty-slot cleanup. Runtime slots remain empty because automatic
failure production and private result-field activation are still pending; these
passes do not prove nonempty source-handle retention or balance. Report evidence
was refreshed. Logs use .scratch/diagnostic-outcome-* and observer-dispatch target
logs. Typecheck, formatting and lint passed for the implementation.

No ticket or OpenSpec task is marked complete. Next work must activate the return
metadata field and preserve NativeResult through every call/outcome consumer,
produce and propagate source handles, and transfer slots rather than releasing a
returned handle. Selected-handler release timing, terminal observation, source
startup/report integration, migration audits and full repository/release gates
remain outstanding.

## Selected recovery suspension and borrowed capture storage

Resolved the catchAll frame investigation above. ProvisionalMir assigned the
protected expression span to both controls, whereas the selected handler's MIR
run includes the enclosing `run` span. The mismatch classified that handler run
as synchronous and omitted its continuation frame. Selected-handler controls now
use the exact run span; the protected control keeps its own expression span.

A stronger native fixture then exposed a separate optimized miscompilation:
recoverInput's deferred child borrowed the failure payload through a stack
allocation that expired when the handler relayed. LLVM consequently treated the
handler argument as poison, and divisor zero incorrectly returned 42 without a
fatal event. SuspensionOwnership now traces borrowed referents through represented
Effect/callable captures and forwarding definitions, for both current run inputs
and live environments. It retains their storage even without a later parent read,
including initialization flags. Capture retention follows the same physical borrow
representation as NativeFunction's address-root discovery. Existing stable frame
placement now supplies the surviving address; no new allocation protocol is needed.

The existing observer analysis test now suspends the protected operation and its
selected handler, checks exact control spans and frame states, and asserts the
failure payload remains in each recovery state. It also holds a deferred borrowed
Effect across another suspension before running it. The native suspended observer
fixture exercises protected and handler suspension before its existing public and
direct suspension boundaries. The receiver reports distinct event/ownership/child
exit failures instead of collapsing them into a single status.

All 47 focused IntrinsicCatalog, SuspensionMir and SelectiveCatch tests passed,
followed by the strengthened live-environment case. All 30 native report lanes
passed on Darwin ARM64 and GNU Linux x86-64/ARM64 in debug and optimized modes.
All 20 LLVM-to-Wasm execution-storage lanes passed. Compact report evidence was
refreshed. Logs are .scratch/recovery-borrow-* and
.scratch/process-admission/recovery-borrow-current-{darwin,amd64,arm64}.log.
Task 5.9 is complete; no ticket is complete. Automatic diagnostic production and
return transport, handler-exit handle release, terminal observation, default source
startup, remaining migration audits, and full repository/release gates remain.

Final typecheck (18 tasks), format:check, lint, strict OpenSpec validation and
git diff --check passed. Lint initially rejected an unnecessary array spread in
the retention loop; it was removed and lint rerun successfully. Full pnpm test,
pnpm check and pnpm release:candidate were not rerun for this increment and are
still required before ticket completion.

## Active private failure-result transport

Activated the diagnostic result ABI instead of leaving every production shape
source-only. In observed modules, potentially failing Effect runners now return
source lanes followed by their six-field owned metadata aggregate, after the
status field for a suspension step. Infallible and public C result shapes remain
source-shaped. NativeCall returns the complete NativeResult. Ordinary, callable,
static and composite consumers accept metadata into the destination outcome slot;
source-only cleanup/callback consumers reject an unexpected metadata field.
Accepting a source-only result clears an existing destination reference, so a
reused outcome cannot retain an earlier failure's context.

NativeReturn takes the selected result slot before releasing temporary outcomes
and the invocation frame. Selective and ordinary propagation supply their exact
source outcome to this completion operation. The transfer header now has eighteen
words: its original twelve words retain the observer and borrowed active cause,
and six additional words hold an owned completed-result record. The new
NativeDiagnosticTransfer actor publishes and consumes that record; consumption
clears the slot before another invocation can overwrite it. Child/resume thunks,
resumed outcome joins and ordinary drivers preserve it. Independently owned
Execution completion explicitly consumes metadata before delivering its payload
to the caller-owned outcome callback.

The existing seeded 32/64-bit result test now verifies nonempty metadata through
transfer publication and consuming clear, and rejects a source-only discard. The
ownership replacement test now enters through the actual result acceptance helper.
All 47 focused tests passed, including these strengthened cases. All 30 native
report lanes and all 20 LLVM-to-Wasm storage lanes passed; compact report evidence
was refreshed. Repository typecheck passed all 18 tasks, and lint passed. Formatting
initially identified two documentation files; they were normalized before the
final formatting check. Logs use .scratch/diagnostic-result-abi-* and
.scratch/process-admission/diagnostic-result-abi-current-{darwin,amd64,arm64}.log.

Task 5.10 is complete, but automatic source failure production remains absent, so
native fixtures still transport empty metadata. Nonempty seeded IR evidence does
not prove end-to-end source handle balance. Next work must produce/propagate handles,
attach selected causes, release handled references at handler exit, and verify
that observer callbacks refresh address-taken state before subsequent source reads.
Terminal observation, default source startup/reporting, migration audits and full
pnpm test/check/release:candidate gates remain outstanding. No ticket is complete.

Final format:check, strict OpenSpec validation and git diff --check passed.
Full pnpm test, pnpm check and pnpm release:candidate were not rerun for this
increment; the final migration gate remains open.

## Authored failure production and suspended union recovery

Connected nominal PackEffectOutcome failure construction and PackEffectFailureUnion
to source observer event 0. Static identity and origin strings now share the
NativeDiagnosticText actor with fatal reporting instead of relying on generated
host-report tables. A selected same-observer borrowed cause adds event 2; the
producer releases the superseded primary reference before publishing the combined
reference. Invocation/scope cleanup and result replacement release owned handles
through their originating observers. Callback emission invalidates cached source
values; NativeFunction reloads address roots after the enclosing operation's
result stores, at its control-flow join. This avoids callback-branch SSA values
being cached into a different control-flow path.

The suspended fixture now owns a source counting observer with nonzero handles.
It checks normal destruction has produced handles and retained none, and checks
fatal observation has exactly one live handle for a selected cause (zero at an
unrelated fatal). Fatal inside a suspended recovery receives the original handle.
A recovery that fails again produces a replacement with the original as cause;
the receiver checks its operands, replacement-reference balance and the resulting
cause after another suspension. Both union failure variants are executed.
This observer verifies the compiler protocol; it is not the bounded Context pool
used by the separate source-adapter fixture, so it does not establish full bounded
reporting or allocation-refusal behavior.

The union fixture exposed two compiler defects and both were repaired. Semantic
analysis previously compared a union-valued fail against individual row members,
despite lowering already supporting PackEffectFailureUnion. It now admits the
fail only when every member belongs to the declared row. The regression retains
SEM0064 and its source span for an undeclared member. Next, SuspensionMir used
find to select only one lowered run per authored control span. Union catch creates
one selected handler call per arm at that same span. Finalization now retains all
such calls, gives additional arms stable distinct continuation ordinals, and uses
the exact operation identity to select each SuspensionOwnership plan. A red/green
regression proved the missing second handler state; both native arms now execute.
Temporary instrumentation used only for diagnosis was removed from dist before
rebuilding the compiler.

All 48 focused compiler tests passed. All 30 native report lanes passed on Darwin
ARM64 and GNU Linux x86-64/ARM64 in debug and optimized modes, including the final
balance assertions. All 20 LLVM-to-Wasm storage lanes passed. Typecheck passed all
18 tasks; lint passed. Compact report evidence was refreshed. Logs use
.scratch/diagnostic-production-_, .scratch/diagnostic-union-_, and
.scratch/process-admission/diagnostic-production-balance-{darwin,amd64,arm64}.log.
Tasks 5.11, 5.12 and 5.13 are complete; no ticket is complete.

Next work still includes propagation frames, allocation-boundary production,
handled-reference release at handler exit, cleanup/callback isolation and bounded
pool refusal/reclamation admission, terminal observation, default source startup
and reporting, migration audits and the full test/check/release:candidate gates.

Final format:check, strict OpenSpec validation and git diff --check passed.
Full pnpm test, pnpm check and pnpm release:candidate were not rerun for this
increment and remain required before completing the requested tickets.

## Outward frames and selective propagation ownership

Native failure propagation now emits observer event 1 with the logical caller's
source label. Replacing the handle publishes the new reference and releases its
predecessor while retaining immutable failure identity and origin. Ordinary static,
stored/composite and selective propagation use the same owned-outcome operation.

Selective propagation previously retained only its narrowed payload local, which
cannot identify the diagnostic metadata owner. PropagateEffectFailure now carries
the original caught outcome separately. Lowering preserves that reference through
union-arm bindings; MIR verification checks it against CatchEffect ownership and
the source failure row. Native return takes metadata from that outcome rather than
the payload. An existing analysis snapshot now also proves that substituting the
payload local for the outcome is rejected as InvalidEffectOperation.

The suspended native fixture checks two ordinary propagation frames and a separate
unselected selective-catch path with four frames, in exact outward order. The later
suspended fatal recovery receives the expected nonzero replacement handle and
exactly one live reference. Normal invocation destruction still requires zero
references. All 30 report lanes passed on Darwin ARM64 and GNU Linux x86-64/ARM64,
in debug and optimized modes. All 20 LLVM-to-Wasm storage lanes passed. The 48
focused compiler tests passed, followed by all 17 SelectiveCatch tests after adding
the forged-owner regression. Typecheck passed 18 tasks; format:check and lint passed.
Compact source/artifact evidence was refreshed. Logs use
.scratch/diagnostic-propagation-* and
.scratch/process-admission/diagnostic-selective-propagation-{darwin,amd64,arm64}.log.

Task 5.14 is complete. Allocation-boundary production, handled-reference release at
handler exit, cleanup/callback isolation, bounded-pool integration and refusal
admission, terminal observation, default source startup/reporting, migration audits
and the full test/check/release:candidate gates remain outstanding. No ticket is
complete. Full pnpm test, pnpm check and pnpm release:candidate were not rerun for
this increment.

## Allocation-boundary production and transfer-state refresh

NativeHostFailure now produces owned metadata through the same outcome production
operation as authored failures, preserving the sealed failure identity, source
origin and selected-cause attachment. Its temporary stack slot exists only on the
immediate failure-return path; it is cleared into the private result before the
invocation releases other outcome references. NativeReturn.completeResult consumes
that owned result, validates the declared source/diagnostic shape, and emits the
synchronous or suspension ABI. Ordinary local returns share this completion path.

Two source conformance cases force alignment-padding overflow in an otherwise
valid layout. The observer checks Intrinsic.StorageFailure production. Recovery
suspends and either reports a fatal with the original nonzero handle and one live
reference, or returns 42 and destroys the observer with zero live references.

The fatal case initially failed with event 201: fatal observation saw handle 1 but
live count 0. A temporary event trace proved there was no intervening Release.
The callee had updated observer state before transferring, but NativeCall refreshed
address roots only on its completion branch. retainRelay then spilled cached zeros
over the updated owner state. Refresh now occurs immediately after the call and
before the completion/transfer branch, so both paths preserve borrowed writes.
The same native case passed after that change. Temporary event tracing was removed;
the C receiver retains failure-only diagnostic output for unexpected exits.

All 48 focused tests passed after the transfer fix. All 30 native report lanes
passed, including both allocation cases, on Darwin ARM64 and GNU Linux x86-64/ARM64
in debug and optimized modes. All 20 LLVM-to-Wasm storage lanes passed. Typecheck
passed all 18 tasks; format:check and lint passed. Compact conformance evidence was
refreshed. Red trace evidence is retained in
.scratch/process-admission/diagnostic-allocation-trace-darwin.log; final native logs
are .scratch/process-admission/diagnostic-allocation-balanced-{darwin,amd64,arm64}.log.
Compiler/Wasm/check logs use .scratch/diagnostic-allocation-*.

Task 5.15 is complete. Handler-exit reference release, remaining cleanup/callback
isolation, bounded-pool integration/refusal admission, terminal observation, default
source startup/reporting, migration audits and full repository/release gates remain
outstanding. No ticket is complete. Full pnpm test, pnpm check and
pnpm release:candidate were not rerun for this increment.

## Release at normal recovery completion

Selected recovery's protected metadata previously remained in its owning outcome
slot until function exit or replacement. The source fixture now uses a direct
Intrinsic.catchFailure followed by an independent C checkpoint in the same function.
Its recovery suspends before returning. The checkpoint requires zero live handles
and must execute exactly once, distinguishing timely release from eventual observer
destruction. Before the fix the Darwin fixture exited 132 at that checkpoint.

MirLinearization now appends ReleaseDiagnosticOutcome after the selected execution's
normal completion operations. The marker follows handler result transfer and runs
before subsequent source work. NativeDiagnosticOutcome clears and releases the
owned slot through its originating observer; unobserved functions have no metadata
storage. A failed handler still follows the existing failure-return cleanup, while
cancellation releases retained frame fields. The existing analysis snapshot checks
two selected completions, their exact caught-outcome owners, and placement at the
end of their recovery blocks. Unselected propagation remains outside those blocks.

The red native checkpoint passed after the change in all six suspended-fixture
target/optimization lanes. All 30 report lanes passed across Darwin ARM64 and GNU
Linux x86-64/ARM64, preserving fatal causes, selective propagation and cancellation
checks. All 48 focused tests and 20 LLVM-to-Wasm storage lanes passed. Typecheck
passed 18 tasks; format:check and lint passed. Compact evidence was refreshed and
the recovery contract was reconciled with the implemented lexical scope. Logs use
.scratch/diagnostic-handler-exit-* and
.scratch/process-admission/diagnostic-handler-exit-{darwin,amd64,arm64}.log; the red
checkpoint is recorded in diagnostic-handler-exit-red-darwin.log in that directory.

Task 5.16 is complete. Remaining work includes cleanup/callback isolation admission,
bounded-pool integration/refusal behavior, terminal observation, default source
startup/reporting, migration audits and full repository/release gates. No ticket is
complete. Full pnpm test, pnpm check and pnpm release:candidate were not rerun for
this increment.

## Payload cleanup and fully handled ordinary catches

The source observer fixture now destroys a CleanupPrimary payload inside selected
recovery. Its destructor produces CleanupNoise and handles it with a direct
Intrinsic.catchFailure. Identity assertions distinguish both failures. The callback
checks WithCause operands 2 and 1; an independent C checkpoint requires three
produced handles and exactly the primary reference remaining at destructor exit.
The fatal variant then suspends and reports the original handle 1, with exactly one
payload destruction. The normal variant suspends, returns 42 and checks zero live
references before subsequent source work, again with one payload destruction.

This initially exposed an unavailable-body placeholder for the destructor, followed
by a pointer/integer call mismatch in LLVM. The destructor's source contract was
valid. EffectLowering.lowerEffectCatch required an enclosing Effect propagation
shape even when it handled every failure. The requirement now applies only to
unselected failure arms. Ordinary functions and destructors can therefore lower a
fully handled intrinsic catch without inventing an enclosing failure result.
A structural regression checks actual CatchEffect operations in both ordinary main
and its destructor; it failed before the change and now passes. The native call ABI
was unchanged because the mismatch was a consequence of the placeholder body.

All 49 focused tests passed. All 30 native report lanes passed on Darwin ARM64 and
GNU Linux x86-64/ARM64 in debug and optimized modes, including both cleanup variants.
All 20 LLVM-to-Wasm storage lanes passed. Typecheck passed 18 tasks; format:check
and lint passed. Compact evidence was refreshed. The red unit result is retained
in .scratch/cleanup-catch-red.log; the initial native failure is in
.scratch/process-admission/diagnostic-cleanup-isolation-darwin.log. Final native logs
are .scratch/process-admission/diagnostic-cleanup-fixed-{darwin,amd64,arm64}.log;
other check logs use .scratch/diagnostic-cleanup-*.

Task 5.17 is complete. Remaining work includes the complete cleanup/callback audit,
bounded-pool integration/refusal admission, terminal observation, default source
startup/reporting, migration audits and full repository/release gates. No ticket is
complete. Full pnpm test, pnpm check and pnpm release:candidate were not rerun for
this increment.

## Source terminal observation runtime

Added the sealed runtime intrinsic Intrinsic.observeUnhandled() -> usize, with an
explicit DiagnosticUnhandled MIR operation, encoding/inspector presentation and
usize destination verification. NativeDiagnosticContext borrows the selected cause,
checks its observer against lexical selection, and dispatches Unhandled through the
owning observer. It neither exposes the payload nor takes the outcome reference.
An absent or mismatched observer is guarded to the disabled dispatch path. Source
retains control over payload cleanup, report formatting and the returned policy.
The intrinsic inventory and accepted semantic fixture were updated.

Two native cases extend payload cleanup: after destroying CleanupPrimary (whose
destructor produces and recovers CleanupNoise), the selected handler suspends and
then invokes the new intrinsic. The independent C receiver requires the original
handle 1, one live reference, three produced handles and exactly one completed
payload destructor before Unhandled. Output must retain CleanupPrimary and its
original origin. A second case closes the report descriptor before source output;
both return policy 42, release the retained context before the following checkpoint,
and destroy the observer normally. This verifies TERM-011 sequencing and policy
independence from output success for the implemented runtime path.

All 49 focused tests passed. All 30 native report lanes passed on Darwin ARM64 and
GNU Linux x86-64/ARM64 in debug and optimized modes. All 20 LLVM-to-Wasm storage lanes
passed. Typecheck passed 18 tasks; format:check and lint passed. Compact evidence was
refreshed. Native logs are
.scratch/process-admission/diagnostic-unhandled-{amd64,arm64}.log and
.scratch/process-admission/diagnostic-unhandled-verified-darwin.log; other check logs
use .scratch/diagnostic-unhandled-*.

Task 5.18 is complete, but the intrinsic's admission is not complete: task 5.19
retains static diagnostics for provably context-free calls and complete verification
of dynamic absence and observer mismatch. These cases must not be treated as proven
by the successful selected-handler native cases. Remaining work also includes
bounded-pool integration/refusal behavior, the complete cleanup/callback audit,
default source startup/reporting, migration audits and full repository/release
gates. No ticket is complete. Full pnpm test, pnpm check and pnpm release:candidate
were not rerun for this increment.

## Conservative terminal-context absence proof

The retained execution graph now records terminal observation sites and selected
handler roots. Both applying a handler and executing its returned Effect seed the
potentially selected closure. SEM0217 rejects sites outside that closure, including
sites reached through ordinary helpers or lazy Effect bodies. A helper shared by
selected and context-free callers remains admitted. Unresolved handlers and selected
callable execution suppress the absence proof instead of producing an unjustified
rejection. The check runs during realization, without building MIR to diagnose
source admission.

The new analysis test checks diagnostic codes and exact intrinsic spans for direct
and lazy calls, plus acceptance of selected-only and mixed-context helpers. A
separate 32/64-bit LLVM structural test proves that terminal dispatch compares the
borrowed cause owner with the current observer, substitutes null on mismatch, and
returns zero through the disabled branch. It also checks event 5 and borrowing of
the existing handle without another callback. The existing selected-handler native
fixtures continue to exercise the active dispatch path.

All 51 focused tests passed. All 30 native report lanes passed on Darwin ARM64 and
GNU Linux x86-64/ARM64 in debug and optimized modes. All 20 LLVM-to-Wasm storage
lanes passed. Typecheck passed 18 tasks; format:check, lint, strict OpenSpec
validation and diff whitespace checks passed. Compact report evidence was refreshed.
Logs use .scratch/terminal-context-* and
.scratch/process-admission/terminal-context-{darwin,amd64,arm64}.log.

Task 5.19 remains open. This initial proof deliberately overapproximates edges that
clear context; fresh observations, independent execution and callback boundaries
need a more precise admission audit. Bounded-pool integration, default source
startup/reporting, migration audits and final repository/release gates also remain.
No ticket is complete. Full pnpm test, pnpm check and pnpm release:candidate were
not rerun for this increment.

## Terminal-context boundaries and dynamic absence

The static absence proof now keeps a separate set of inherited diagnostic edges.
Running a fresh observation does not carry the enclosing selected cause into its
protected body. Argument construction and the state/callback owner's cleanup after
scope exit retain the enclosing context. A regression exposed a missing owner-drop
edge, now recorded explicitly. A new handler inside the fresh body still supplies
its own selected failure.

The independent-execution regression exposed another missing edge: drive outcome
callbacks inherit the driver context even though the body starts independently.
The graph now includes completion/suspension callbacks, park registration and shared
access callbacks. Dynamic readiness notification includes every retained endpoint
as a possible target. Represented callback identities use completed invocation
lifetime arguments; unavailable targets suppress the absence proof. Tests distinguish
an independent body from selected completion and readiness callbacks.

The suspended native observer fixture now shares terminalPolicy between selected
recovery and five disabled contexts. Mode 17 probes after the handled outcome has
been released, mode 18 inside a fresh observer across suspension, mode 19 at C entry,
mode 20 inside the observer callback, and mode 21 inside an independent Execution
across suspension. Independent C assertions require policy zero and no second
Unhandled event. Where the primary is retained, the subsequent report still carries
handle 1, CleanupPrimary and its original origin. The fresh observer receives no
event and is dropped exactly once; its cleanup sees the retained primary still live.

The fresh-scope rejection test failed before the edge distinction; the driver
callback and owner-cleanup acceptance cases each exposed and verified a missing
edge. The corresponding red logs are .scratch/terminal-boundary-red.log,
.scratch/terminal-independent-tests.log and the initial owner-cleanup assertion
recorded during this increment. Final focused logs use
.scratch/terminal-boundary-{focused,cleanup,tests}.log. Temporary graph tracing was
removed.

All 53 focused tests passed. All 30 native reporting lanes passed on Darwin ARM64
and GNU Linux x86-64/ARM64 in debug and optimized modes, including all five dynamic
absence cases. All 20 LLVM-to-Wasm storage lanes passed. Typecheck passed 18 tasks;
format:check and lint passed after fixing a nested-ternary style error. Compact
report evidence was refreshed. Final native logs are
.scratch/process-admission/terminal-boundary-verified-{darwin,amd64,arm64}.log;
other checks use .scratch/terminal-boundary-*.

Task 5.19 is complete. Task 5.20 records the next integration step: use compiler-
produced handles with the bounded source context owner, including allocation/pool
refusal and reclamation, before source startup integration. The complete cleanup
and lifecycle audit, default source entry/reporting, migration audit and full
repository/release gates remain. No ticket is complete. Full pnpm test, pnpm check
and pnpm release:candidate were not rerun for this increment.

## Public bounded context owner and cancellation allocation safety

NativeDiagnostics is now a public standard-library actor. It accepts the selected
NativeReport writer and node capacity, owns the lazy observation and reuses slots
when handled contexts are released. Zero capacity, capacity overflow and allocation
refusal preserve semantic identity/origin without optional context. Nodes and a
bounded iterative traversal stack share one reservation. Reporting and reference
release neither allocate nor recurse. Context destruction checks that all compiler-
owned references have been consumed before the reservation is freed.

The native context fixture now uses real compiler-produced primary identities,
outward frames and selected causes. Manual protocol construction was removed.
Its receiver checks allocation refusals, tiny pools, repeated handled failures,
output bounds/EIO, unstarted drop, parked cancellation and fatal reports. Every
free poisons the released allocation; normal and cancellation exits require all
allocations released exactly once.

The poisoned cancellation case exposed an execution-package use-after-free.
Cancellation had published the final cancelled Wake phase before dropping frames.
A Wake released by frame cleanup could therefore free the package while its
storage owner was still needed. LLDB located the poisoned accounting pointer in
ExecutionStorage.destroy after the cleanup callback. NativeExecutionOperation now
owns cancellation as one bracket: publish a cleanup phase, drop frames and cached
endpoints, then consume the updated Wake state and release or retain the package.
Dropping or notifying the Wake during cleanup records consumption without freeing
the package. An escaped Wake retains the final release obligation. Duplicated
caller-side release decisions were deleted.

All 30 native reporting lanes passed on Darwin ARM64 and GNU Linux x86-64/ARM64 in
debug and optimized modes, including both Wake destructor paths and actual fatal
reports. All 20 LLVM-to-Wasm storage lanes and 79 focused tests passed. Typecheck
passed 18 tasks; format:check and lint passed. Compact context evidence now names
only the actual integrated cases and includes the public owner and cancellation
implementation digests. Logs use .scratch/context-owner-final-* and
.scratch/process-admission/context-owner-final-{darwin,amd64,arm64}.log. The red
cancellation evidence is .scratch/process-admission/context-owner-lifecycle-darwin.log
and .scratch/context-owner-lldb*.log.

Task 5.20 is complete. Source-entry integration is in progress under 5.21. Adding
the owner to the existing source-entry fixture exposed a return-metadata ownership
error in the HostInput application lane after ordinary/unit/suspended entry passed.
This remains under diagnosis; default entry installation, generated-policy removal,
migration audits and final full-repository/release gates remain unfinished. No
ticket is complete. Full pnpm test, pnpm check and pnpm release:candidate have not
been rerun for this increment.

## Source-entry reporting and transfer-only result ownership

The selected source-entry fixture now installs NativeDiagnostics inside the owned
application Execution. This respects the independent invocation boundary instead
of borrowing a driver observer. Source supplies capacity 64, 4096 output bytes and
32 logical frames. Its selected terminal handler destroys the payload before
requesting Intrinsic.observeUnhandled and returning the source policy.

The HostInput suspension lane exposed a missing metadata owner in a generated
provided Effect.suspend wrapper. The wrapper uses the suspension ABI but originates
a child transfer without a resumable frame. NativeFunction had allocated metadata
slots only for nonsuspendable invocations or persistent frames, leaving this third
case empty. It now allocates invocation-local slots whenever no invocation frame
exists. Persistent frames retain their existing ownership. NativeReturn's lost-owner
invariant remains enforced. The minimized source and red/green codegen logs are in
.scratch/entry-report-{repro.silk,check.mjs,minimal-suspend-service.log,fixed.log}.
Temporary compiler instrumentation was removed.

A new failure after a suspended service call then exposed Effect.flatMap discarding
the original context through Result and raising the payload again. Its implementation
now directly runs self and then the success callback, so an incoming failure
propagates without replacing its identity/origin. The same native case subsequently
exposed generated provider names in origins. NativeDiagnosticText now accepts the
MIR function and uses its structured effectRunner base before rendering the authored
name and source span. This preserves source positions without generated provider or
effect-site counters. The rejected reports are recorded in
.scratch/process-admission/entry-report-{fixed,propagated}-{darwin,amd64,arm64}.log.

All 48 source-entry lanes passed on Darwin ARM64 and GNU Linux x86-64/ARM64 in debug
and optimized modes. They cover ordinary/unit/suspended calls, HostInput across
suspension, captured returned Effects, ordinary failure and suspended service failure.
Success emits no output; failures retain authored identity/origin and policy 1.
The compact hosted-start-conformance.json also checks that selected source objects
contain no generated silk_main or legacy failure/cause/trap-report symbols. These
claims apply to this selected composition; the default generated path still exists.

After the compiler changes, all 30 native report lanes and 20 LLVM-to-Wasm storage
lanes passed again, together with 56 focused tests. Typecheck passed all 18 tasks;
format:check, lint, generated-documentation policy/check, strict OpenSpec validation
and diff whitespace checks passed. The standard-library reference was regenerated,
including NativeDiagnostics, and compact report/Wasm evidence was refreshed. Native
logs use .scratch/process-admission/entry-report-verified-* and
.scratch/process-admission/entry-report-regression-_; other final checks use
.scratch/entry-report-_.

Task 5.21 is complete. The audit identified two concrete follow-ups recorded in
6.2.1 and 6.3.1: other Effect combinators still use Result-to-raise paths that can
change diagnostic context, and the generated reference includes private provider
conformance headings (Node, Cursor and Context in NativeDiagnostics). Docgen source
was inspected but not changed in this increment. Default runtime selection,
bootstrap/scheduling policy, deletion of generated reporting/entry, full migration
closure and final repository/release gates remain. No ticket is complete. Full
pnpm test, pnpm check and pnpm release:candidate were not rerun for this increment.

## Public documentation visibility for runtime helpers

The NativeDiagnostics reference exposed private Node, Cursor and Context
implementation headings even though their declarations were excluded. Docgen's
Project model had marked every conformance as inherited visibility without checking
its provider or interface. It now inspects resolved nominal types throughout both
heads and marks a conformance private when it mentions a private declaration.
Public project models filter those items; includePrivate retains them explicitly
marked Private. Public conformance documentation remains available.

The existing reference regression was extended with a private provider, private
interface and private generic argument, while retaining a public provider/interface
implementation as a positive check. The Project regression confirms explicit
private output retains the private conformance. The new public-reference assertion
failed before the fix (.scratch/reference-visibility-red.log), and all 10 focused
Project/Reference tests passed afterward. The full docgen suite then passed all 68
tests in 12 files. Typecheck passed all 18 tasks, and format:check, lint, regenerated-
reference consistency, strict OpenSpec validation and diff whitespace checks passed.
The regenerated NativeDiagnostics page was inspected and contains only its public
actor and operations; the private implementation headings are gone. Logs use
.scratch/reference-visibility-*. Task 6.3.1 is complete. This does not close the
overall JUL-147 audit or replace the pending full repository/release gates.

## Installed hosted startup and exact callable execution

The hosted conformance runner now selects the installed `silk/native_start` module.
The duplicate fixture-local entry source is deleted. Source startup owns the C main
contract, process-input snapshot, application Execution, result-interface conversion,
and application diagnostic pool. Bootstrap has a zero-capacity observer and typed
failure policy 1. A source generic NonParking bound admits nested transfer while
requiring application source to own external parking; it does not add a scheduler.
Default selection and generated entry/report deletion remain open under 5.1–5.5.

The new bound exposed three general compiler defects. ExecutableOrigin omitted exact
callable representation identities while resolving applied generic parameters.
Native callable application bypassed the ordinary completion driver and attempted to
relay without suspension control. MIR origin verification also omitted callable
targets held in local types. Finally, ordinary selected interface calls did not add
execution dependencies, hiding ExternalPark from the NonParking proof. Interface
calls now share ordinary call-origin specialization, including executable arguments.
These corrections use existing callable/interface facts, with no startup-name rule.

The minimal nested callback failed native code generation with “lost transfer
control” before the fix and now emits LLVM successfully. Committed structural and
LLVM regression coverage lives in Suspendability.test.ts. A separate interface
parking regression went from no diagnostics to SEM0139 plus the independently
applicable SEM0140 entry diagnostic. The existing HashMap mutation test now also
expects the correctly discovered entry diagnostic. Explicit NativeStart analysis
rejects the parking application with SEM0139 at its source generic invocation.

All 48 installed-module entry lanes passed on Darwin ARM64 and GNU Linux x86-64/ARM64
in debug and optimized modes after the final interface correction. Compact evidence
is refreshed in hosted-start-conformance.json; logs are
.scratch/process-admission/native-start-interface-{darwin,amd64,arm64}.log.
The ordinary/unit/nested-transfer/HostInput/returned-Effect/failure cases retain
source reporting and no generated entry/report symbols. Bootstrap allocation fault
injection and reentrant whole-entry calls are still pending, not claimed by these lanes.

Generated library references now include NativeStart. Required repository typecheck
passed all 18 tasks, and strict OpenSpec validation passed. The full repository test,
check and release-candidate gates remain pending for the overall migration.

The complete source-entry bootstrap fault fixture subsequently passed all six
Darwin ARM64/GNU Linux x86-64/ARM64 debug/optimized lanes. Its independently compiled
C constructor calls the source C main repeatedly, refusing every allocation ordinal
observed in a successful run, both with working writes and EIO. Source bootstrap
refusal returns policy 1 before the application body; optional diagnostic allocation
refusal permits the body to complete. The harness checks reclaimed and poisoned
allocations, invalid argc/argv contents, nested entry preserving outer allocations,
and application failure-payload Drop before report output. It exits 42 before CRT
would call main again. No constructor was added to the production runtime.

The new test:hosted-start-faults command and required native CI matrix retain this
coverage and its .scratch/hosted-start-faults artifacts. Exact C/Silk/generator and
header pins are in startup-fault-conformance.json and startup-supplies.json. Logs:
.scratch/process-admission/startup-fault-run-{darwin,amd64,arm64}.log.

Final targeted verification passed 77 compiler tests across seven files and all
20 shared Wasm storage/lifecycle lanes. Documentation policy checked 85 modules with
no violations, and generated-reference checking passed. Full repository pnpm test,
pnpm check and release:candidate are still required after the remaining migration.

## Distribution defaults and removal of compiler entry policy

Executable defaults now select the installed NativeStart or WasmStart source module.
Target/libc selections and execution-storage bindings live in stdlib/compositions.json;
the generated distribution catalog and toolchain catalog identity include those inputs.
No semantic or lowering actor recognizes those library module names. Explicit libraries,
objects and runtime-none profiles acquire no default startup/storage. Default standalone
Wasm has executable artifact semantics and exports its source C main through export-dynamic.

Runtime.invoke, resolved invocation roots, Instances.Entry, MIR entry adapters and
CloseEffectEntry, machine-entry symbol rewriting, generated C main/report functions,
process-global failure/cause state and their native emission branches are deleted.
The obsolete entry-shape and unowned-entry diagnostics are deleted; source call,
conformance, requirement and NonParking checks own admission. Artifact-root verification
accepts empty libraries while still checking every retained/exported implementation.
The inspector and CLI describe ordinary roots/outcomes. Lexical runtime diagnostics retain semantic source metadata. Obsolete artifact-level
Termination tables, cache fields and the public Termination export are also deleted.

After these deletions, all 48 explicit installed NativeStart lanes passed on Darwin
ARM64 and GNU Linux x86-64/ARM64, in both optimization modes. Compact evidence in
hosted-start-conformance.json is refreshed from removed-entry-{darwin,amd64,arm64}.log.
Public Driver probes returned 42 on Darwin and Wasm in both modes, with source C main
and no silk_main export (.scratch/removed-entry-default-probe.log). The removed-entry bootstrap fault rerun passed all six native lanes, and the
removed-entry Wasm rerun passed all 20 shared storage/lifecycle lanes. Compact
startup-fault and storage-wasm evidence is refreshed. These matrix runs preceded
the later deletion of unused artifact Termination metadata.

The focused compiler suite passed 106 tests in six files before the distribution-table
and convenience-API follow-ups (.scratch/entry-removal-tests-final.log). Distribution
selection passed a separate structural test across all four supported targets. A new
regression exposed that implicit-target analysis convenience APIs chose their target
after loading the frontend; they now select it first so source startup roots are loaded.
Both convenience paths pass the regression (.scratch/default-analysis-api-test2.log).
One newly added test passed runtime verification but needed Uint8Array.from for the
SourceFile byte array; that type correction is applied and subsequent repository typechecking passed.

AnalysisFixture.retainingMain supplies explicit object/retention/storage composition for
language-only graph tests. The initial five migrated suites passed after replacing the
legacy MIR entry golden line with escaped retained-root identities. A further 97 suites passed 849 tests across the initial and repaired runs
(.scratch/language-fixture-tests.log and .scratch/language-fixture-repair-tests.log);
.scratch/language-fixture-files.json records the exact set. This is test-only composition, with no production main-discovery fallback.

Generated references include WasmStart and omit retired entry diagnostics. Program-entry,
artifact-root, runtime and reserved-symbol references now describe source ownership.
Other documentation, test consumers, source/artifact inventories, Effect combinators,
full repository checks and release-candidate verification remain open. No ticket or
whole-goal completion is claimed by this increment.

After deleting artifact-level Termination metadata, repository typechecking passed all
18 tasks (.scratch/source-entry-final-typecheck.log). The focused final source-entry,
Backend, EffectEntry, Usize and UnicodeNormalizationConformance run passed 36 tests
in five files (.scratch/final-source-entry-tests.log). Native EffectEntry uses the
configured LLVM toolchain, verifies source trace frames, and preserves status one
when stderr is closed. The CLI report fixture needed its renamed MIR-rule expectation
updated; its rerun remains pending.

The full parallel compiler suite is now running as a migration diagnostic pass.
It has exposed declaration-only/default-startup fixtures and frontend realization
without retained roots in OsFileSystem, IntrinsicCatalog and StdlibResolution.
Those fixtures are being repaired with explicit test compositions; no production
main discovery or startup bypass has been restored. Program termination, typed
failure, runtime/provider and hosted/storage inventories now describe source-owned
startup and lexical reporting rather than the deleted generated adapter.

## Source-runtime consumer and documentation audit

The parallel compiler diagnostic run finished with 2,348 passing tests and 45 failures
in 19 of 210 files (.scratch/source-runtime-parallel-suite.log). Most failures were
legacy implicit-entry fixtures or whole-artifact assertions that now included source
startup. Explicit object/retention profiles migrate those language assertions; editor
assertions now request frontend analysis, and native toolchain fixtures expose an
ordinary C entry. The first repair run passed 320 tests in 18 files with five remaining
failures: four timeouts and a phase-report count expectation. Further repairs are under
verification; neither run is represented as a clean repository gate.

The algorithmic retained-root golden passed after inspection of its four intended
functions and escaped root identity (.scratch/source-runtime-algorithmic-test.log).
All 13 CLI report tests passed after deletion of artifact Termination metadata and
renaming the MIR root rule (.scratch/source-runtime-cli-report-repair.log).
Documentation policy passed 86 modules and generated-reference checking passed
(.scratch/source-runtime-doc-policy.log and .scratch/source-runtime-doc-check.log).
Two unused imports found by lint were removed; the latest typecheck/lint reruns remain
necessary after these fixture edits.

coverage-ledger.md records every WS/SPEC and JUL-137–146 remainder, using read-only
Linear retrieval on 2026-09-08. JUL-148/149/150 invariants have explicit replacement
owners. intrinsic-inventory.json records all 699 sealed operations with the compiler's
admission category, consumer, signature, target and execution surfaces. This is an
inventory awaiting independent privilege review, not an automatic approval of every
primitive. source-absence-audit.json records file digests and zero matches for the
specified retired actors/operations and hardcoded new source-runtime module names in
non-generated compiler/CLI source. Artifact absence remains a separate open check.

The expanded Wasm runner now includes seven source-entry cases in both optimization
modes. Preliminary runs exposed invalid test captures and private error types in the
new fixtures; these were corrected through ordinary source analysis. No incomplete
run updates the compact conformance evidence. The complete expanded matrix is pending.

The completed expanded Wasm run passed all 34 lanes: eight independent storage probes,
twelve Driver lifecycle/refusal cases and fourteen default source-entry cases, in both
optimization modes (.scratch/source-runtime-wasm-entry-matrix3.log). Every module is
import-free; source entry returns the specified status or traps, and the retired entry
export is absent. The payload-destructor trap distinguishes cleanup from status-only
failure handling. storage-wasm-conformance.json is refreshed from this completed run,
including the new fixture digest. Task 5.4 is now checked; broader command/reactor
composition remains JUL-145, and the remaining native/tooling/closure gates remain open.

Follow-up ABI inspection reopened task 5.4: LLVM rewrote the zero-argument `main`
into a two-argument C wrapper and exposed `__original_main`. The 34-lane run above
proved outcomes but did not prove entry arity; its evidence is insufficient for
completion. An isolated LLVM probe confirmed exact-symbol spelling preserves the
source signature. The backend now applies that spelling consistently to Wasm
foreign symbols, and the runner asserts zero arity and rejects the wrapper export.
The corrected matrix is pending.

The final focused repair run passed all 49 Instances and OsFileSystem tests
(.scratch/source-runtime-repair-tests3.log). Together with the preceding focused
runs this repairs the 45 failures from the diagnostic suite; a single complete
repository gate has not yet been rerun.

The corrected Wasm matrix passed all 34 lanes with explicit zero-arity assertions
(.scratch/wasm-symbol-matrix.log), and all 20 Backend tests passed
(.scratch/wasm-symbol-tests.log). Source-entry exports contain neither silk_main nor
__original_main. The compact evidence is refreshed from this corrected run and task
5.4 is checked again. Subsequent Effect combinator source changes require focused
validation and the final runtime gates; this does not complete the broader migration.

Source combinator propagation now uses direct run for tap and acquired providers;
mapBoth composes map/mapError, and mapError invokes ordinary selected recovery. The
catch primitive's structured contract and catalog, plus catch/catchAll wrappers,
accept a once Effect from their once-invoked handler. The inventory delta is exactly
that one signature. Retry now materializes only attempts that will be retried, drops
those payloads before another attempt, and directly propagates the final attempt.

The captured mapper exposed two lowering gaps. EffectLowering's result lookup and
both selected-handler call sites now use Layout.callableTargetArguments, retaining
captured callable identities. NativeFunction's stable coroutine parameter store now
uses NativeType.addressLaneOffset for concrete callable/Effect environments. Earlier
runs failed with OrphanSuspensionMachinery and an undefined callable lane offset;
both faults are fixed, not suppressed. Temporary dist instrumentation was restored
and the compiler rebuilt before recording passing conformance.

The focused regression run passed 123 tests in SelectiveCatch, Elaboration and
ZipAcceptance (.scratch/combinator-capture-tests.log). All 19 SelectiveCatch tests
passed again after adding LLVM emission to the captured union-handler regression
(.scratch/combinator-capture-codegen-test.log). The updated intrinsic inventory test
passed; the prior 38 other IntrinsicCatalog tests passed before the lowering fixes.
Compiler test typechecking passed (.scratch/combinator-test-typecheck2.log).

All 60 native source-entry lanes passed across Darwin ARM64 and GNU x86-64/ARM64,
in debug and optimized modes (.scratch/process-admission/combinator-layout-entry-*.log).
The added tap fixture preserves a failure origin after suspension and skips a
trapping success callback. The mapped-failure fixture retains its selected primary
cause while consuming a captured callback. hosted-start-conformance.json is refreshed
from the complete reports. The refreshed 34-lane Wasm run also passed with zero-arity
assertions (.scratch/combinator-wasm-matrix.log). Documentation generation and the
86-module policy check passed. The 293-file source-absence audit remains clear, and
strict OpenSpec validation passed (.scratch/combinator-openspec-validation.log).

Ensuring still converts through Result and loses the original diagnostic owner. The
transparent finalization seam is now specified in design.md and the hosted-runtime
delta; task 6.2.2 records implementation, all-engine semantics and parked cleanup as
pending. No finalization intrinsic has been implemented. Root typecheck is running;
format/lint, the complete test/check gates and release candidate remain outstanding.

The subsequent root typecheck passed all 18 tasks
(.scratch/combinator-root-typecheck.log). The first format check found only the two
refreshed conformance JSON files; after formatting those files, the root format check
passed (.scratch/combinator-root-format-check2.log). Root lint passed
(.scratch/combinator-root-lint.log). These are current preliminary gates; pnpm test,
pnpm check and pnpm release:candidate still have not passed for the completed change.
No conformance or diagnostic process remains running from these checks.

Transparent finalization is now implemented (task 6.2.2 remains under verification).
Intrinsic.finalizeEffect admits two consuming Effects with an infallible unit finalizer;
Effect.ensuring uses it directly. Lowering holds the original success or failure and
its diagnostic outcome owner, runs the finalizer without a selected-recovery scope,
and transparently propagates the original failure. Distinct operand spans preserve
protected/finalizer suspension identities. Metadata-only propagation references no
longer acquire an erased ordinary value slot in a coroutine frame.

The parked-destruction fixture exposed a shared-payload cleanup call that omitted
private diagnostic arguments. NativeAggregate now invokes that generated helper
through NativeCall, as it does Drop hooks. The native cancellation fixture checks
exact payload destruction, balanced nonempty diagnostic handles, observer destruction,
and absence of a selected-cause edge for a failure handled inside the finalizer.
It passes for protected success and typed failure
(.scratch/finalization-native-observed3.log; two focused native cases pass).

Finite Effect choices use an explicit inert capture projection followed by ordinary
conditional execution and the existing invocation/suspension paths. The choice case
uses distinct capture layouts and suspension in both inputs. EffectJoin verifies
MIR and LLVM emission. A further narrowing case, mixing infallible and different
failure-row arms, is being checked before finalization is closed.

The exported cancellation fixture also exposed allocation provenance that relied on
a materialized Effect environment. Direct forwarding may omit that environment.
LocalSharedAllocationProvenance now follows concrete incoming call arguments and
uses that index for its existing parameter-origin fixed point. Darwin/Wasm exported
fixtures, including reentrant callbacks, pass admission. The combined IntrinsicCatalog,
OwnedAllocationAcceptance and EffectJoin run passed all 54 tests
(.scratch/finalization-contract-tests.log), and compiler test typechecking passed
(.scratch/finalization-tests-typecheck.log). The checked intrinsic inventory now has
700 operations, including the finalization seam.

The expanded native storage and hosted-start matrices are running from the current
compiled implementation. The hosted-start finalization fixture independently checks
that the finalizer completed exactly once before failure-payload Drop, and that the
reported primary identity/origin survives suspension without a new selected cause.
The initial expanded Wasm matrix passed its choice/cancellation lanes; its completed
report must be refreshed after the narrower choice regression is resolved. Source
comments and prescriptive finalization documentation have been updated; generated
references, complete repository gates and final closure audit remain outstanding.

Finalization narrowing is now verified for an infallible arm and distinct failure
rows. The check exposed valid LLVM IR with later-listed defining blocks that the
bitcode writer rejected. LLVM 22.1.8's independent assembler accepted that IR.
FunctionEncoder now writes unsigned relative operands and forward-reference types
according to the upstream record contracts. The seven LLVM output/round-trip tests
pass, including fixed and variadic calls, load/store and a forward return operand
(.scratch/finalization-forward-roundtrip.log). Four focused compiler finalization
cases pass after that repair (.scratch/finalization-forward-native.log).

The strengthened cancellation probe initially failed every independent native/Wasm
receiver despite returning 42 through normal startup. A C allocation log isolated
one leaked Shared owner when the protected body parks before producing an outcome.
SuspensionOwnership had derived cleanup from an erased Effect type. ConcreteCleanup
now owns the exact environment cleanup shared with ordinary lowering, preserving
unrun finalizer captures in suspension slots. All 26 ownership/stored-Effect/join
checks pass (.scratch/concrete-cleanup-tests.log). The reused allocation-provenance
snapshot now also asserts the retained finalizer's reclaiming EffectCleanup; all
11 tests pass (.scratch/concrete-cleanup-regression.log). This is a repaired defect,
not a waived harness failure.

All refreshed required storage and startup lanes passed: 36 native storage lanes,
40 Wasm lanes (10 independent probes, 16 lifecycle cases, 14 source-entry cases),
and 66 native source-entry lanes (.scratch/process-admission/concrete-current-*.log,
.scratch/concrete-current-wasm.log). The independent finalization probe checks
cancellation before the protected outcome and while finalization is parked,
nonempty diagnostic ownership, original payload retention, no selected finalizer
cause and exact release of captured allocations. Compact evidence is refreshed
only from those completed reports.

The bootstrap walk now includes implicit malloc/free/memcmp source exports and
Shared/Execution/Wake cleanup. Fourteen storage admission tests pass, including
recursive source malloc/free replacements (.scratch/storage-bootstrap-closure-tests.log).
privilege-audit.md reviews all 700 sealed operations by category and individual
new boundary, plus the source-component and external-helper bootstrap contracts.
The 294-file source absence scan has no findings. Exact old-symbol object/export
scans are recorded separately in artifact-absence-audit.json. Documentation generation,
the 86-module policy check and strict OpenSpec validation pass.

Backend emission identity is v9. Root typecheck passed after the concrete cleanup
repair. The first following lint check found an unused import left by extraction;
it was removed and lint passed. Full repository test/check and release-candidate
gates remain outstanding; tasks.md 6.4 is not complete.

The first root test gate passed LLVM's 75 tests and parity validation, then stopped
at docgen: two selected API-only fixtures still requested default executable
profiles, and the live native doctest sweep exceeded its old 180-second budget.
The fixtures now explicitly request object/entry-none profiles; all eight reference
tests pass (.scratch/migration-docgen-reference-repair.log). A complete instrumented
58-example sweep found three obsolete fragments without main, and no other errors
(.scratch/profile-doctests.log). Before concurrent compiler validation began, the
first 11 examples took 34.2 seconds; every example now also analyzes source startup.
The live sweep budget is 300 seconds, with no timeout changes to other tests.

Clock and Random documentation now use complete, canonically formatted deterministic
provider examples. Random explicitly labels its provider as a test double unsuitable
for secrets. Both programs pass analysis and actually return 42 through source native
startup (.scratch/check-new-examples.log; .scratch/run-new-examples.log). The redundant
FileSystem service fragment is replaced with precise provider and partial-directory
cleanup prose; its complete module example remains. No ignore fences were added.

The complete compiler parallel suite finished with 2415 passing tests and two failures
(.scratch/migration-compiler-parallel.log). One expected report location moved from
silk/effect:297:18 to 294:18; its exact-byte expectation is updated. The other was the
source/generated-stdlib drift caused by the documentation edits made during this
validation run; regeneration is pending. Neither is waived. The next root gates run
only after source and generated documentation have been synchronized.

The regenerated documentation now passes the full 68-test docgen suite, including all
57 active Darwin examples. The two compiler failures from the overlapping source edit
are resolved: generated source equality and the current Effect source origin both pass
in the focused 25-test run (.scratch/migration-compiler-repairs.log).

The subsequent root gate passed typecheck, format and lint, then exposed a project
frontend omission in LSP: default startup composition roots were not loaded before
profile selection. Frontend.selectProject now loads the same default composition as
single-root compilation and carries the inferred or explicit application identity into
closure selection. Both native and Wasm project regressions pass, including a helper
listed before the explicit application (20 ProjectAnalysis tests). LSP assertions now
include source startup and distinguish the initial closure from conditional imports.

Cold inspector realization measured approximately four seconds for the selected
source runtime, exceeding the ordinary two-second query deadline. Inspection now has
its own ten-second deadline; ordinary editor query deadlines are unchanged. The
cross-file navigation test uses the shared sixty-second correctness timeout for its
multiple source-runtime revisions. Focused shared-project, real-stdio inspector and
cross-file invalidation checks all pass (.scratch/migration-lsp-final-focused.log).
The full ordered gates are being rerun; no final gate is waived.

The maintained audit now has a repeatable entry point,
packages/compiler/scripts/check-native-migration-audit.mjs. Its source pass verifies
all 294 current non-generated compiler/CLI files and their reviewed bytes. Its
--artifacts pass verifies all preserved report hashes and repeats 124 native
inspection and 40 Wasm export checks. The exact retired-symbol inventory now has
41 names, including static generated helpers from both deleted baseline actors and
the previously removed standard-stream adapter. Both passes have zero findings
(.scratch/migration-repeatable-audit.log). The ledger records the exact worktree
baseline, complete WS/SPEC numbering, CI/local-evidence distinction, unsupported
LTO and deferred managed/raw facilities. Strict OpenSpec validation passes.

The current root run has passed all 68 docgen tests, generated-documentation checks,
86-module documentation policy, and all 53 CLI standard-library doctests with zero
skips or failures. The full compiler suite is running; final gates remain open.

The final inspection consistency pass found that ProjectAnalysis views did not retain
the requested configuration and Inspection rebuilt them with a default native
profile. JUL-130 remains open at task 5.5 while this owner-level tooling fix is
verified. Views now retain the original profile/composition/bindings; Inspection
forwards them to reconstruction and derives its emission mode from the normalized
profile. The existing Wasm settings test now checks that inspection selects
silk/wasm_start and excludes silk/native_start. Tasks 6.1 and 6.3 are complete;
5.5 and the final repository gate remain open.

The full compiler parallel suite passed all 2,419 tests in 210 files
(.scratch/migration-final3-test.log). The subsequent serial native corpus was stopped
explicitly to rebuild the inspection configuration fix; that root run is incomplete,
not a passing gate. Native acceptance also no longer performs a duplicate preliminary
Analysis pipeline merely to assert that MIR exists: Driver.compile performs the actual
checked compilation, and the separate shared LLVM verifier suite retains structural
coverage. This reduces the source-startup cost without dropping any native execution
case or expected outcome. Fresh compiler/LSP builds completed before focused inspection
verification; the next ordered gates run on those completed sources.

### Native acceptance gate: cleanup joins and exact runner isolation

The final6 repository run passed typecheck, formatting and lint, then passed all
2,419 parallel compiler tests. Its full native corpus gate reported 292 passing
and 32 failing cases; `pnpm check` and `pnpm release:candidate` were not reached.
These failures invalidate closure, rather than being waived as unrelated fixtures.

The common LLVM dominance failure came from synchronous Shared payload cleanup
reloading addressable roots inside its last-reference branch. Cleanup joins now
reload both mutable and addressable roots; the Shared and composite Effect joins
also establish fresh dominating values. Three focused native regressions passed.

Cancellation cleanup had also copied retained frame initialization flags into the
emitting caller's local map. An owner's flag ordinal could overwrite an unrelated
callback argument with an integer. Cleanup now supplies the cancelled frame's flags
in a separate map, preserving the caller's locals. Final suspension control also
excludes an exact synchronous recovery runner when it shares the source span of a
suspendable protected recipe. The structural regression and surrounding suspension
suites pass (11 tests across three files).

Repeated diagnostic text is interned per emitted function, allowing multiple
propagation paths through the same source site. Native corpus configuration now
supplies package ownership for package parameters; the libc fixture agrees with
source storage's pointer declarations. Five stderr fixtures include the outward
source startup propagation frame and current source spans. A focused run of all
32 previously failing native cases is in progress; no passing full gate is claimed.

The focused native repairs now cover all 32 original failures: 20 passed in the
first rerun, the libc declaration fix passed separately, and all 11 initially
unmatched long names passed after the harness fix. Vitest's interpolated object
labels truncated long case names; the harness now registers full names directly,
uses `it.effect`, and filters explicitly selected cases before registering tests.
No early-return case is reported as executed. The compiler test typecheck and lint
pass with this harness.

The fixed backend also passes all 36 refreshed native storage lanes and all 66
refreshed native startup lanes across Darwin/aarch64 and GNU Linux/x86_64/aarch64.
The Wasm conformance refresh is in progress. Backend cache identity is now v10;
source audit hashes cover the repaired actors. The full ordered repository gates
remain required, and final6 remains a failed gate in the historical evidence.

### CLI gate: Wasm profile migration and truthful watch fixtures

The final7 gate passed typecheck, formatting and lint, plus all 68 docgen and 159
LSP tests. CLI reported four failures (83 passed), so the compiler test task and
later gates were not completed. The mixed-target batch still synthesized the old
Wasm `loadable-module` profile despite requesting an executable WebAssemblyModule.
BuildBatch now uses ArtifactKind's canonical profile mapping for both targets, with
an assertion in its existing ordered-batch test.

Two single-file fixtures forced `/usr/bin/clang`; Apple's LLVM 17 reader rejected
attribute kind 102 emitted by the admitted LLVM 22 backend. These fixtures now
select the available LLVM toolchain consistently with the other native tests.
The watch fixture also performed repeated full semantic compilation while using a
400 ms quiet interval as its idle oracle. Source-owned startup made an in-flight
pass outlast that interval, so a later old-source result was mistaken for the empty
edit. Writer-settlement cases now record the exact source passed to the compilation
callback directly. Separate existing tests continue to exercise actual checking,
diagnostic recovery and edits during active analysis. Focused CLI validation is in
progress; no final7 passing closure is claimed.

### Full compiler gate: three correctness deadlines

The final8 run passed all 87 CLI tests and repeated the compiler's 86-module policy,
generated documentation check and 53 published examples. The parallel compiler run
finished with 2,417 passing tests and three confirmed `Test timed out in 60000ms`
failures: the Fiber documentation program (63.017 s), native final-cache admission
(60.069 s), and the confined native filesystem provider (60.025 s). There were no
reported assertion failures. The same cases passed in the earlier final6 gate;
this run cannot be called passing, and native acceptance was not reached.

These three larger cases now have explicit two-minute correctness deadlines; the
workspace default and all assertions are unchanged. The Fiber exception applies
only to that complete scheduler walkthrough. Native cache admission requires its
cold compile, reused compile and rejected-supply compile, and filesystem admission
compiles the complete source provider before exercising a real confined root.
Focused execution and a fresh ordered gate must complete before closure.

### Landing-page module analysis and complete native acceptance

The final9 gate passed all 2,420 compiler tests, all 324 native acceptance tests,
87 CLI tests, 159 LSP tests, and 68 docgen tests. It then failed the docs app's
landing-page snippet assertion: a declaration-only Effect example acquired the
Wasm executable startup and consequently diagnosed its absent `main`.

The live snippet element and its landing-page verifier now explicitly analyze an
object profile with no runtime. This preserves declaration diagnostics without
installing application startup. The element's existing passing-example assertion
now covers the declaration-only Effect example, so the browser path is checked
alongside all 15 landing-page semantic snippets. Final repository gates are repeated
in `migration-final10-*` logs; final9 is retained as failure evidence.

### Packed compiler export contract

The final10 repository gates passed, including `pnpm check` and its 17 script tests.
Release-candidate validation passed nine of ten checks and found that the explicit
packed compiler export list still expected the removed `Termination` actor and
omitted the new `RuntimeComponent` actor. The expectation now names the intended
public surface; it continues to compare the complete sorted export list exactly.
The final11 sequence repeats all required gates after this test-only correction.

### Final passing handoff

The final11 sequence exited successfully after `pnpm typecheck`, `pnpm format:check`,
`pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`, in that order.
Type checking passed 18 tasks, repository tests passed 22 tasks, and the combined
check passed 33 typecheck/test tasks plus all 17 repository script tests. Unchanged
package results were reused from Turbo's verified cache: the full 2,420-test compiler
suite and 324-test native corpus passed in final9, and the final editor/docs changes
passed in final10. All 10 release-candidate checks executed and passed in final11.

The source/artifact absence verifier also passed after the final production change:
294 compiler/CLI modules, 124 native inspections and 40 Wasm export sets have no
retired paths or symbols. The final checklist is complete. `verification.md` records
the delivery, exact gate logs, provenance and remaining roadmap scope. No remote CI
result, issue status change, publication, commit or OpenSpec archival is claimed.

### Stacked PR integration

The implementation snapshot is committed as `12686586`. Main advanced to `7cc09e57`
with contextual character literals, lexer cleanup/documentation, compact import
formatting and generated untracked toolchain integrity. Merge resolution preserves
those changes and the migration's explicit module profiles. Post-merge typecheck,
format, lint, 129 focused character/lexer/formatter tests and the refreshed source
and preserved artifact audit pass. Full post-merge repository/release gates are
running in `pr-merge-*` logs; the pre-merge final11 results remain scoped to their
original revision. The PR stack separates the coherent implementation and reference
updates from JUL-147's audit verifier, inventories, ledger and verification report.

### CI failure reproduction and repair

PR #387's first CI run (`34248071207`) exposed four verification problems:

- The live Debian security index no longer served `linux-libc-dev=6.1.180-1`.
  A clean Docker build reproduced the exact package-resolution failure. The three
  pinned conformance Dockerfiles now use Debian and Debian-security indexes frozen
  at `20260907T000000Z`, preserving every existing package version. Clean ARM64 and
  x86-64 builds pass; all 12 filesystem header hashes match on each architecture.
- The entry-free filesystem conformance object exercised suspended execution without
  selecting execution storage. Its exact analysis request reproduced `SEM0214`
  (`MissingParameter: execution-storage`). Explicit source component bindings repair
  that request; the complete Darwin receiver passes in debug and optimized modes.
- Native suspension and nested-row tests bypassed `SILK_TEST_CLANG` and invoked LLVM
  18 on CI. Those tests and the same configuration mistake in temporary-directory
  acceptance now use `TestToolchain.configured`. All eight affected tests pass with
  LLVM 22. A recording compiler wrapper also proves the configured executable is used.
- The aggregate 53-example standard-library doctest exceeded its five-minute Linux
  deadline. Its bounded deadline is now ten minutes; examples, diagnostics and the
  no-skips requirement are unchanged. The full gate verifies the sweep again.

The first post-merge local run failed because a concurrent rebuild temporarily
removed `dist`; this was verification sequencing, not a compiler result. A subsequent
local run was deliberately stopped to incorporate newly completed CI failures. The
complete replacement sequence runs in `ci-fix2-{typecheck,format,lint,test,check,release}`
logs. Type checking, formatting and lint pass; full test/release results remain pending.
Implementation fixes are committed in `a4fde63a` and carried into the audit stack.

### CLI workflow budget after the second CI run

Replacement CI run `34249675709` passed the previously failing compiler shard 1;
shards 2 and 4 also passed. The complete documentation sweep passed in 385,963 ms,
confirming the old five-minute deadline was below its real CI cost. Validation then
exposed a distinct timeout in `Cli.test.ts`: init/check/build/run drove three compiler
pipelines under the one-build 60-second budget. The original test reproduced the
60-second timeout locally while the full compiler suite was running. With all source,
execution and status assertions unchanged, the integration now receives three
single-build budgets. The focused run passed and printed `Hello, world!`; CLI test
type checking, formatting and lint also pass. The stale sub-second timing rationale
in the single-build timeout module is updated to the current source-startup cost.

### Shared data imports and cold LSP analysis

CI run `34251502269` passed all four compiler shards, native shards 1/3, browser,
macOS native OS, and all three full platform-supply lanes. Native shard 2 rejected
the `foreign-libc-environ-static` corpus case with `SEM0192`: the GNU source runtime
and fixture both import `environ`. Matching the source pointer contract alone still
failed because ForeignPlanning rejected every duplicate data import. A focused
regression reproduced that planner failure. Commit `58564933` accepts matching C
data imports, emits one LLVM global and artifact record, and retains typed/span
assertions for incompatible imports, function/data collisions and export collisions.
All 55 ABI/planning tests pass. The exact GNU executable codegen request now succeeds
with the matching fixture contract; the old incompatible pointer contract still fails.
The source-absence hashes for ForeignPlanning and NativeProgram were refreshed after
review; preserved object receipts retain their original provenance.

The same CI run retired healthy LSP workers during cold source-runtime analysis.
A deterministic virtual-clock regression reproduces a second worker spawn after the
old ten-second no-progress lease. The new 30-second lease retains the worker while
ordinary query/diagnostic/startup/inspection deadlines remain unchanged. All 24 engine
tests pass, including actual stalled-worker retirement. All 16 stdio tests pass with
a bounded 120-second multi-revision budget and 60-second per-response budget. No
assertions or test cases were removed. The audit PR's validate failure reproduces
the same LSP problem.

The `ci-fix2` ordered typecheck/format/lint/test sequence passed (2,428 compiler tests,
324 native acceptance tests, 87 CLI tests and the remaining package suites). Its
subsequent check was deliberately stopped after new compiler edits invalidated those
inputs. This is partial validation, not a completed final gate. The final stack
sequence runs in `ci-fix3-{typecheck,format,lint,test,check,release}.log`. Focused checks
and strict OpenSpec validation already pass; full test/check/release and replacement
remote CI results remain pending.
