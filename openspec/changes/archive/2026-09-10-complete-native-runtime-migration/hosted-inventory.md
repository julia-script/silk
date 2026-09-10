# JUL-130 hosted boundary inventory

This records the implemented source boundary and the evidence still needed for final closure.
The historical generated adapter is deleted. Source/tool/supply evidence is recorded in the
adjacent conformance JSON files; repository-wide verification remains a separate gate.

## Selected startup and process inputs

The distribution’s `stdlib/compositions.json` selects `silk/native_start` for hosted native
executables and `silk/wasm_start` for standalone LLVM-to-Wasm. ArtifactComposition reads the
catalog generically; semantic analysis and lowering recognize no startup module spelling.
Runtime descriptors name source modules and contain no invocation field. Runtime source imports
`Intrinsic.application`, makes ordinary calls, and defines C exports. Foreign exports and explicit
retained declarations seed the executable closure. Empty libraries and retention-only objects
need no application entry. Libraries, objects, and runtime-none profiles acquire no defaults.

NativeStart exports C `main(argc, argv)`, owns a NativeHostInput snapshot and an Execution,
and supplies mutable HostInput through OsHostInput when requested by the application Effect.
Its source interface accepts integer, unit, or Effect-unit results. Generic NonParking admission
allows nested transfers inside that Execution and rejects an application that externally parks
without its own owner. No compiler entry-shape rule or `silk_main` adapter remains.

| Operation          | Implemented source responsibility                                                                        |
| ------------------ | -------------------------------------------------------------------------------------------------------- |
| Argument count     | Reads the owned invocation snapshot                                                                      |
| Indexed argument   | Preserves absence, empty presence, and opaque bytes independently                                        |
| Environment lookup | Copies platform environment inputs under the unsafe capture contract; preserves the first matching entry |
| Working directory  | Owns bounded getcwd growth, captures errno before release, and cleans every allocation                   |

OsRuntime, OsCall, the four host-input intrinsics, generated argc/argv globals and their
reserved names are deleted. NativeHostInput and OsHostInput own lifetimes explicitly, including
foreign library callers and nested invocations. No process-global initialization flag or library
constructor substitutes for input ownership. See host-input-contract.md for exact byte, capture,
and copy contracts and host-input-supplies.json for native declarations.

WasmStart exports C `main() -> i32`. It owns an Execution, returns application integers unchanged,
returns zero for unit/Effect-unit success and one for typed failure after dropping the payload.
It installs no reporter or host input; traps remain bare machine traps. The linker preserves
source C exports with export-dynamic rather than naming a compiler-generated entry symbol.

## Report metadata and source policy

Immutable failure identities, source locations and trap reasons remain semantic compiler facts.
NativeDiagnosticText emits them where the selected lexical observer needs them. Diagnostic
context belongs to each owned failure outcome. NativeDiagnosticScope establishes the observer;
ordinary calls, selected recovery, suspension and cancellation obey the ownership rules in
report-context-contract.md and the exact transport in report-observer-contract.md.

NativeDiagnostics owns a bounded persistent context graph and iterative release/render policy.
NativeReport owns formatting, byte/frame limits, descriptor writes and status policy. NativeStart
selects 64 nodes, 4096 output bytes and 32 frames for application diagnostics. Bootstrap uses a
zero-capacity observer and allocation-free fallback. A typed failure’s payload is dropped before
terminal observation; allocation refusal and output failure preserve status one. Fatal traps do
not unwind or promise successful output.

Generated C main/report text, mutable failure/path/cause globals, trap callback reservations and
artifact-level Termination tables/API are deleted. A disabled observer contributes no report
allocation or output. Foreign C entry establishes an independent unobserved private invocation;
source may then install an observer. A separately owned parked Execution cannot retain a departed
driving observer. Callback adapters disable recursive observation.

## Prior-art comparison

Inspected local Zig revision
`e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa`: `lib/std/start.zig`
(`main`, `callMainWithArgs`, `callMain`), `lib/std/process/Args.zig`,
`lib/std/process/Environ.zig`, and standalone `empty_env`/`env_vars` fixtures.
Zig passes an argument vector and environment block into source initialization,
then builds owned allocator/IO/environment facilities there. Its POSIX arguments
are opaque bytes; its environment tests distinguish missing and empty values and
values containing equals. This supports explicit source inputs and owned copies.
Silk does not adopt Zig's root-main recognition or global allocator policy.

Inspected local Rust revision
`c33d8f3b5a50b56466998e8c5ed8a077d2caed84`: `library/std/src/rt.rs`,
`library/std/src/sys/args/unix.rs`, `library/std/src/sys/env/unix.rs`, and
`tests/run-make/c-link-to-rust-staticlib/{rmake.rs,bar.c,foo.rs}`. Rust stores GNU
arguments globally, including an init-array extension; Apple queries libc accessors.
Its environment reader uses a process lock and copies bytes, while its runtime
separates initialization, main/payload disposal, cleanup and exit coordination.
Silk adopts explicit lifetime and cleanup obligations, not global startup state,
panic unwinding, or a library constructor. The static-library fixture reinforces
testing a foreign caller without language executable startup.

These implementations are comparisons, not target ABI authorities. Pinned libc
headers, independent C declarations/objects, actual entry calls, and debug/optimized
execution remain required admission evidence. The targeted entry/environment test comparison is recorded below; it distinguishes
source inspection from executed Silk conformance.

## Source entry specialization prerequisite

A source wrapper may specialize the application result without recognizing a library
name in the compiler. The hosted-entry probe exposed missing identity evidence when
an interface witness receiver contains an Effect with the same open failure row on
both sides. Open witness inference must bind that row's member parameters to themselves,
just as it does ordinary type parameters, and must reject conflicting earlier evidence.
This is a prerequisite inference correction, not a new entry intrinsic or relaxation
of the prohibition on suspending C exports. End-to-end source invocation is exercised by the selected source-entry fixture
described below.

## Source C invocation admission

The installed `stdlib/silk/native_start.silk` module defines ordinary C
`main`, selects the application through `Intrinsic.application`, and runs it inside
an owned Execution. This preserves the existing prohibition on directly suspending
foreign exports. Source interface policies convert integer, unit and fallible Effect
results; no generated `silk_main` invocation participates in this fixture.

Two additional compiler defects were exposed by actual emission: static interface
witness calls did not specialize on executable operand identities, and addressable
Effect values looked up their erased public type instead of their concrete environment.
Discovery now routes those operands through ordinary call-origin specialization;
witness lowering uses the resolved instance arguments. Native address storage and
lane offsets use concrete Effect/callable environments, including nested captures.
These are general representation corrections, not entry-name recognition.

This module is the installed hosted executable default and remains explicitly selectable. A generic
NonParking bound admits nested transfer but rejects an application whose execution
closure can externally park. The application must establish its own owner for parking.
An unexpected parked callback destroys the execution and produces a typed bootstrap
failure. NativeDiagnostics owns observation inside the
independent application's Execution, using an explicit pool, byte and frame bound.
The terminal handler drops its payload and returns Intrinsic.observeUnhandled's
policy. Bootstrap uses a zero-capacity observer and the same typed-failure status 1.
The complete bootstrap allocation-ordinal sweep now passes with successful and failed
report writes, invalid arguments, payload cleanup and reentrant entry on all three
native targets in both build modes. The independent C harness poisons released storage
and rejects leaks and duplicate frees. Its constructor invokes the source C entry and
exits before CRT invocation; this is test machinery, not production initialization.
Evidence is startup-fault-conformance.json with pinned headers in startup-supplies.json.
The generated hosted adapter and superseded fixture-local source entry are deleted.

The 66 current source-entry lanes cover integer/unit results, suspension, explicit
HostInput across suspension, captured returned Effects, success and typed failure
including tap, captured error mapping and transparent finalization on Darwin ARM64
and GNU Linux x86-64/ARM64 in both optimization modes. A failing
suspended HostInput computation preserves its original identity and authored origin
through Effect.flatMap. Compact evidence is hosted-start-conformance.json.

This integration exposed two transport/presentation defects and one source-library
defect. Transfer-only runners without resumable frames now receive invocation-local
metadata slots. Effect.flatMap uses direct run sequencing instead of discarding
context through Result and producing another failure. NativeDiagnosticText uses the
runner's structured base declaration so provider specializations do not expose
generated counters in report names.

## Source formatter admission

`silk/native_report.NativeReport` now owns terminal text formatting and explicit
byte/caller-frame limits. Each value borrows a descriptor, allocates nothing, and
uses NativeDescriptor's admitted partial-write/EINTR boundary. It accepts canonical
identity, origin, caller and cause strings; it never reflects on failure payloads.
Typed failure and fatal classifications remain distinct. Any exhausted limit or
output failure prevents later writes, including causes after a truncated frame list.

The native-report fixture passed all six debug/optimized target lanes. Each lane's
independent C receiver exercises seven exact-byte/status cases: normal typed/cause
output, fatal output, byte refusal, committed-prefix failure, EINTR, zero progress
and frame truncation. Pinned header evidence lives in report-supplies.json; detailed
object, toolchain and execution evidence is under .scratch/native-report.

Formatting and semantic transport are implemented together. The report observer, context,
suspended, cancellation, hosted-start and startup-fault evidence files record distinct executed
slices. The final audit must keep each slice’s source digest and coverage limits explicit; a
formatter-only run is not evidence for outcome transport or startup lifetime.

## Exact entry and environment test comparison

The pinned local checkout revisions were rechecked before reading these tests.
`hosted-prior-art-tests.json` records exact file digests. This is source inspection;
neither upstream test suite was executed as Silk admission evidence.

Zig's `test/standalone/entry_point/build.zig` builds the same x86_64 Linux module
with entry symbols foo and bar, then checks that the emitted files differ. Its
own comment limits the claim to frontend entry selection and cache inclusion;
it deliberately chooses one target where that path works. `main.zig` suppresses
the normal startup declaration and exports two different integer-returning functions.
This supports independently keyed entry selection, but proves neither a usable
hosted C main contract nor cleanup, argc/environment lifetime or reporting. Silk's
two-profile test must inspect selected roots, retained symbols and requirements,
then execute its selected hosted entry; a binary-difference assertion alone is
insufficient. Silk keeps explicit runtime/application roots instead of adopting
Zig's root declaration convention.

The Environ tests at that Zig revision cover POSIX block construction, map insertion
and overwrite, case-sensitive lookup outside Windows, map/block round trips,
missing versus empty values and owned allocated lookup. They support Silk's byte
snapshot and absence/empty distinction. Silk deliberately preserves the first
matching entry in the input vector instead of normalizing it into an overwriting
map. Its existing C fixture additionally proves duplicate/malformed entries,
non-UTF-8 bytes, mutation after capture, committed prefixes and untouched tails.
Windows case folding and WTF-8 behavior are outside the admitted native targets.

Rust's `library/std/tests/env.rs` at the pinned revision checks current-directory
availability, executable-path shape, path-list split/join semantics and debug
formatting for argument/environment iterators. It does not test raw-byte snapshots,
duplicate environment precedence, short-buffer copying, startup lifetime or
concurrent foreign mutation. Those claims therefore rely on Silk's independent
fixtures and explicit unsafe capture contract, not on this upstream test file.
Path-list parsing is not part of this slice and is not added to HostInput.

The Rust C-to-staticlib fixture compiles an exported empty Rust function, links a
C main against the archive, executes it, removes the archive and executes again.
It explicitly excludes cross compilation. This is useful evidence for the shape
of an independent foreign consumer and for independence from the archive at run
time. It does not establish absence of constructors/globals, reentrant report
state, error transport or Silk's cross-target ABI. Silk must inspect its own
library symbol/requirement closure and exercise independent invocations with
source-owned state. The pinned C headers, LLVM tools and actual target lanes
remain the ABI authorities.

## Native result boundary inventory for diagnostic transport

The ordinary MIR completion paths now converge on `NativeReturn.complete`:
`NativeControl` handles Return and selective propagation; `NativeEffectOperation`
handles direct, composite and stored-Effect propagation; `NativeHostFailure` handles
allocation failure. The operation checks the declared lane count and selects the
synchronous result ABI or a completed suspension step. The result ABI now adds a separate metadata field for potentially failing Effect
runners in observed modules. Authored nominal/union and allocation-boundary failure production are connected.

| Boundary                               | Existing representation                                                                | Implemented ownership                                                                                      |
| -------------------------------------- | -------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| NativeDeclare ordinary result          | Source-shaped result, with a trailing metadata aggregate for observed fallible runners | A failing Effect outcome transfers its own metadata; ordinary source payload layout stays independent      |
| NativeCall.callSynchronous             | Returns NativeResult with separate source lanes and transferred metadata               | Capture the called outcome separately from cleanup and other calls                                         |
| NativeReturn.complete                  | Takes returned metadata before releasing other outcome slots and completing            | Transfer only the selected returned failure; release temporary contexts                                    |
| NativeSuspension.returnStep            | Status, source lanes and optional metadata; completed return releases the frame        | Move returned metadata before frame release; transfer statuses must not pretend to own a completed outcome |
| NativeSuspension child/resume thunks   | Publish source lanes and metadata into distinct transfer storage                       | Retain no pointer to a departed native stack; preserve the originating observer                            |
| NativeSuspension driver                | Takes completed metadata, destroys storage state, and returns the complete result      | Transfer result metadata before destroying storage; its observer must belong to an enclosing live scope    |
| NativeExecutionOperation               | Consumes result metadata before delivering the independent outcome payload             | Keep outcome metadata distinct from package/body cleanup and release it on consumption or cancellation     |
| NativeDeclare C export thunk           | Public C signature with a null hidden observer on the private call                     | Preserve the C signature and establish an independent unobserved invocation                                |
| NativeDiagnosticScope callback adapter | Ordinary source callback with a null observer                                          | No recursive observation or accidental borrowing of an invocation's selected cause                         |

The result plumbing retains NativeResult, assigns transferred metadata to outcome locals, takes
returned context before other cleanup, and moves it through the owned transfer slot across
child/resume/driver paths. Independent executions consume metadata at completion. Production,
propagation frames, selected-handler exit timing and source terminal observation are connected;
there is no generated report-state fallback. The Result/rethrow combinator and retained privilege audit is recorded in
privilege-audit.md; full repository verification remains tracked in tasks.md.
