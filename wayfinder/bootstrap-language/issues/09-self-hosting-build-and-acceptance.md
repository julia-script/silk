# Define the staged self-hosting build and acceptance procedure

Type: grilling
Status: resolved
Blocked by: 05, 06, 07, 08

## Question

What exact stage-0, stage-1, and stage-2 build procedure proves that the Silk Effect compiler can
compile its own source into a working native compiler without Node.js or TypeScript at runtime, and
what conformance, determinism, artifact, and target checks make that milestone unambiguous?

## Answer

Self-hosting is an all-or-nothing native acceptance procedure over one content-addressed source
snapshot and explicit per-target build recipes. It proves that a compiler written in Silk Effect
can compile that same compiler source, drive the already selected external LLVM/Clang pipeline, and
produce a working fixed-point native compiler without Node.js or TypeScript at runtime. It makes
that claim relative to declared trusted seed and native-toolchain inputs; it does not claim diverse
double-compilation assurance or independently reproducible LLVM, SDK, or libc builds.

### Stage graph

The compiler stages are named by how each executable was produced:

1. **Stage 0** is the trusted Effect/TypeScript seed compiler executed by the pinned Node.js seed
   runtime. It compiles the canonical Silk compiler sources and asks the pinned Clang toolchain to
   emit and link the first native compiler.
2. **Stage 1** is that first native compiler. In the hermetic native environment it compiles the
   same source snapshot and drives Clang and the native linker itself to produce stage 2.
3. **Stage 2** is the self-hosted native compiler produced by stage 1. It compiles the same snapshot
   once more to produce a verification-only fixed-point rebuild.

The verification rebuild is not a separately distributed stage 3. Stage 2 and the verification
rebuild must be byte-identical at every required release artifact boundary. Stage 1 remains build
evidence; only stage 2 may become the accepted compiler candidate. References in earlier planning
to a "stage-1 TypeScript frontend" mean the stage-0 TypeScript frontend under this terminology.

### Declared inputs and trust boundary

Every stage consumes a **build recipe**. The recipe is the canonical identity of all inputs that
must remain equal between the stage-1-to-stage-2 build and the stage-2 fixed-point build. It records:

- a sorted source manifest containing the exact bytes and normalized relative paths of every
  compiler, bootstrap standard-library, runtime, and C-shim input;
- the target triple, target layout, CPU and feature floor, codegen profile, logical source/build
  roots, and fixed output leaf names;
- resolved paths, SHA-256 digests, versions, and complete ordered arguments for Clang, the selected
  assembler and linker, LLVM inspection tools, and any archiver;
- Clang's normalized and effective target triples, resource directory, runtime-library selection,
  and complete `-###` command expansion;
- the SDK or sysroot identity and digest, startup and runtime objects, system-library identities,
  deployment target, and dynamic-linker choice; and
- a minimal environment allowlist with every allowed value. Tools are resolved by recorded absolute
  path rather than ambient `PATH`; locale and timezone are fixed, and network input is forbidden.

The **source snapshot** is the content-addressed source bundle named by the recipe, not a Git
revision. It contains only manifest-listed regular files. Symlinks, unlisted generated inputs,
untracked files, checkout location, filesystem traversal order, and Git metadata cannot affect the
build. A required generated file is either part of the snapshot or the output of an explicit
recipe step whose declared inputs and output digest are also recorded. A Git revision is useful
provenance but is neither the source identity nor a native-stage dependency.

Each invocation produces a separate **build record** containing the recipe identity, producing
compiler digest, emitted artifact digests, canonical diagnostics, exit status, child commands,
phase and process measurements, and host/tool inspection results. Producer digests legitimately
differ between stages; the recipe does not.

The trusted base consists of two content-addressed input families:

- the materialized stage-0 JavaScript bundle, pinned Node.js executable, and all stage-0
  dependencies; and
- one immutable native dependency bundle per host containing Clang/LLVM tools, linker inputs,
  SDK or sysroot, platform startup objects, compiler-rt inputs, and declared system libraries.

Fixed-point equality is evidence relative to this base. It neither proves that the seed compiler
is free of a trusting-trust attack nor that the native dependencies can be independently rebuilt
to the same bytes. Diverse double compilation and full supply-chain reproducibility are later
milestones.

### Hermetic native execution

Stage 1 and stage 2 run in a clean, networkless environment where the stage-0 seed, Node.js,
TypeScript, pnpm, Git, shells, and undeclared tools are not mounted or otherwise executable. This
is stronger than removing them from `PATH`. Only the source recipe, stage output area, Silk bundle,
and pinned native dependency bundle are visible. Undeclared filesystem access or process execution
invalidates the run.

The native compiler loads the source closure, emits bitcode, invokes Clang to emit the object, and
invokes the native linker itself. The external acceptance harness may materialize declared inputs,
launch and measure a stage, hash and inspect outputs, and execute fixtures. It may not emit compiler
artifacts, transform one stage into another, invoke Clang or the linker on the compiler's behalf, or
generate an undeclared source input. Child-process auditing must show only the native compiler and
recipe-declared tools.

### Conformance corpus and compiler equivalence

A closed requirements-to-fixtures manifest defines the minimum corpus. Every normative bootstrap
decision from issues 01 through 08 maps to at least one stable fixture ID. Applicable rules receive
both accepted and rejected cases, with edge cases for ownership, borrowing, cleanup, typed
failures, services and witnesses, effect reuse and suspension, affine allocation and Drop, imports and cycles,
incomplete-source recovery, host boundaries, target ABI, and deterministic ordering.

Fixtures belong to six classes: compile-and-run, compile-and-reject, operational-failure, trap,
analysis-recovery, and debug/native-inspection. Expected diagnostics compare stable codes, spans,
severity, related facts, and edits rather than prose alone. Fixture bytes, expected outputs, stable
counts, and hashes belong to the build recipe. Semantic traceability, not a line-coverage
percentage, determines completeness.

Acceptance uses two equivalence tiers:

- **Reference equivalence:** stage 0, stage 1, and stage 2 produce byte-identical canonical syntax,
  HIR, and MIR encodings, structured diagnostics, standard output, standard error, and exit status
  for every relevant fixture. Successfully generated programs have identical observable behavior.
  Stage-0 and native backend artifacts need not have the same bytes because the seed and native
  implementations may serialize equivalent native code differently.
- **Native equivalence:** stage 1 and stage 2 additionally produce byte-identical release bitcode,
  objects, and executables for every corpus program under the same recipe. This extends the native
  fixed-point claim beyond the compiler's own source.

The native compiler has three ordinary process outcomes:

- status `0`: compilation succeeded and the requested durable artifact was committed;
- status `1`: source rejection produced deterministic diagnostics and no output artifact; and
- status `2`: an operational or configuration failure, including filesystem, process, stream,
  toolchain, or out-of-memory failure, left no partial durable artifact.

A trap remains abnormal termination rather than being translated into one of these statuses.
Acceptance asserts abnormal termination and absence of a committed artifact without requiring one
portable numeric signal status. The compiler writes a requested durable artifact transactionally;
status `0` is impossible before the final rename or equivalent commit succeeds. Allocation-free
emergency reporting, including a broken standard-error path, still ends with status `2`.

### Fixed-point and artifact determinism

The blocking fixed-point comparison uses the release profile with debug metadata disabled. Stage 2
and its verification rebuild must have byte-identical canonical syntax/HIR/MIR encodings, raw LLVM
bitcode, relocatable objects, and linked executables. LLVM bitcode is not a universal semantic
normal form, but raw equality is the correct gate here because the two runs use the same Silk
writer, source snapshot, recipe, and canonical logical paths.

Repeat the stage-2 build in a second clean physical directory mapped to the same logical roots and
require the same bytes again. A mismatch blocks acceptance. Disassembly, `llvm-dis`, `llvm-diff`,
metadata inspection, and structured diffs may diagnose it but cannot weaken or waive the gate.
Debug and release-with-debug profiles must still produce deterministic canonical frontend
encodings and bitcode; their native object and executable gates are behavioral debugging checks
rather than fixed-point byte comparisons.

The C shim is compiled with date/time macros rejected and physical paths mapped to fixed logical
roots. Profiles exclude PGO, profile instrumentation, LTO, `-march=native`, `-mcpu=native`, and
random identifiers. Linux links use explicit content-derived `--build-id=sha1`. macOS retains the
linker's content-derived `LC_UUID` and required deterministic ad-hoc signature. Identity-based
development or distribution signing happens only after comparison and never feeds a signed file
back into the fixed point.

### Native and smoke targets

Run the complete stage-0-to-stage-2 fixed-point procedure independently on native runners for:

- `arm64-apple-darwin`;
- `x86_64-unknown-linux-gnu`; and
- `aarch64-unknown-linux-gnu`.

Each runner compiles, links, inspects, and executes its stage compilers and generated corpus
programs. Cross-compilation and CPU emulation do not satisfy a native gate. Inspect the object
format, machine, target and deployment metadata, linked dependencies, runtime shim, and executable
before running it.

One declared runner also performs non-blocking object-emission smoke tests for
`x86_64-pc-windows-msvc` COFF and `wasm32-unknown-unknown` WebAssembly objects. Pinned inspection
tools verify their format, machine, and required symbols. These jobs are reported explicitly as
object-emission smoke evidence and make no runnable-host claim.

The GNU/Linux ABI floor is glibc 2.28 on an EL8 userland for both architectures. Separate immutable
x86-64 and Arm64 sysroots pin exact package, repository, signing-key, sysroot-manifest, and OCI-image
digests. Use conservative CPU floors `-march=x86-64 -mtune=generic` and `-march=armv8-a` unless a
later accepted change raises them.

Every stage-1, stage-2, fixed-point, and release ELF must:

- use the declared interpreter for its architecture and an exact `DT_NEEDED` allowlist;
- contain no undeclared `RPATH` or `RUNPATH`;
- contain no `GLIBC_PRIVATE`, unexpected unversioned, undeclared `GCC_*`, or `GLIBC_*` requirement
  newer than `GLIBC_2.28`; and
- execute the full native bootstrap and corpus on a digest-pinned, fully patched native EL8 machine
  or VM with loader, libc, kernel, and image identities recorded. A container sharing a newer host
  kernel is supplemental evidence rather than the required compatibility runner.

Run the unchanged EL8-built artifacts on a maintained newer-glibc userland as supplemental
forward-compatibility evidence. A deliberately too-new-symbol fixture must be rejected by both the
static ABI audit and the EL8 loader, proving the floor check is active. Review the floor before EL8
maintenance ends in 2029; never retain an unmaintained sysroot silently.

### Native debugging

Every required native host runs a pinned batch-debugger fixture. A debug build must accept a source
breakpoint on a known Silk function, stop at the expected canonical module and line, and step to the
next Silk statement. A controlled trap must show the expected Silk callee and caller names with
source locations. Repeat stack inspection under release-with-debug to prove useful line-level
traces at `-O2`. Inspect the DWARF directly as well so debugger presentation drift can be separated
from malformed metadata. Full optimized locals and source-type visualization remain deferred.

### Performance and resource acceptance

At first, one dedicated non-shared Arm64 macOS machine is the sole blocking performance reference.
Its hardware fingerprint, OS image, power and thermal configuration, and toolchain recipe are
recorded. Changing them requires an explicit rebaseline record. Linux jobs record the same metrics
without adding independent bootstrap performance thresholds; correctness, determinism, and native
execution remain blocking on every host.

Compare stage 0 and release stage 2 as fresh processes using the compiler source snapshot and a
frontend-only request ending after MIR. The measured frontend interval includes source loading
through completed MIR and excludes LLVM emission, external Clang, and linking, which are recorded
separately. Run three warmups and nine measured trials per compiler with measured order
deterministically shuffled and filesystem caches warm. Stage 2's median frontend time and median
process peak RSS must each be no greater than stage 0's. Record per-phase times, allocator metrics,
process peak RSS, input/output counts, diagnostic counts, and artifact sizes. Thermal throttling,
background load, recipe drift, or excessive trial variance invalidates rather than passes a run.

Three content-addressed corpus pairs enforce scaling:

- 128 versus 256 reachable independent modules;
- 2,048 versus 4,096 declarations with module count fixed; and
- 1,024 versus 2,048 fixed-shape function bodies with modules and declarations fixed.

For every affected frontend phase, measure the empty-root startup baseline using the same trial
protocol and compute `(median doubled - median startup) / (median base - median startup)`. The ratio
must not exceed `2.5`. If adjusted measurements are too close to timer resolution, the run is
invalid and the fixture must be enlarged before acceptance. Memory scaling is recorded but the
settled `2.5` blocker applies to elapsed phase time.

Generated-code performance uses paired Silk and C `-O2` fixtures for scalar operations, aggregate
access, direct calls, checked arithmetic, typed failure propagation, scoped cleanup, and
service-witness dispatch. Each pair performs the same observable work and checks and produces the
same checksum under identical target/CPU settings. Run three process warmups and fifteen measured
in-process samples with Silk and C deterministically interleaved. Fixed recipe iteration counts
make each sample last at least 250 ms. The Silk median must be no more than `2.0` times its C median
for every fixture individually. Reject optimized-away work and any C comparison that omits a
semantic check Silk performs.

Resource acceptance uses deterministic fail-at-allocation-ordinal sweeps on one small request that
reaches each compiler phase, continuing until the first uninjected successful run. Every injected
failure must surface as typed `OutOfMemoryError`, exit with status `2`, run exact LIFO cleanup, return live
logical allocation bytes to the pre-request baseline, and leave neither temporary nor durable
artifacts. Representative file-system, child-process, and standard-stream failures use test
providers and obey the same guarantees. After every failure, a successful compilation in the same
test process proves state was not poisoned. Allocator metrics and an ordered finalizer event log,
not process RSS alone, prove cleanup.

### Accepted bundles and promotion

Each target produces three content-addressed outputs:

- the **Silk toolchain bundle** containing the stage-2 compiler, bootstrap standard-library inputs,
  version-matched runtime/shim object, target specification, and file manifest;
- the **native dependency bundle** containing the pinned Clang/LLVM tools, linker inputs, SDK or
  sysroot, platform startup and compiler-rt objects, and dependency manifest; and
- the **acceptance evidence bundle** containing build recipes and records, retained stage and
  fixed-point hashes and intermediates, canonical phase encodings, corpus results, ABI and debug
  inspections, performance measurements, command logs, and the final gate report.

Only stage 2 is promoted. Stage 1 and the byte-identical verification rebuild remain evidence.
Promotion happens only after every blocking gate passes for the same source snapshot and declared
per-target recipes. All three native hosts must pass before any bundle is called an accepted
self-hosting release. Individual-host outputs remain unaccepted candidates.

Every gate reports `pass`, `fail`, `invalid`, or `not-run`. A missing, skipped, stale, or invalid
blocking result is not a pass, and blocking gates cannot be waived. Changing sources, expected
outputs, toolchain/sysroot inputs, the performance reference, or a threshold creates a new recipe
and requires the affected acceptance chain to run again. Non-blocking Windows and WebAssembly smoke
failures remain visible without falsifying the native self-hosting claim.

The primary-source basis for the byte-comparison, platform-metadata, toolchain-provenance, and Linux
ABI decisions is recorded in
[`self-hosting-reproducibility.md`](../research/self-hosting-reproducibility.md).
