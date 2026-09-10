## Context

See proposal.md for scope. The updated baseline contains source descriptors, filesystem, clocks, entropy, selected artifact roots, native assembly and post-object helper accounting. This change replaces the remaining execution storage, child process, host-input and hosted entry/report C policy with source components, including shared LLVM-to-Wasm storage and source entry.

## Default source compositions

Executable hosted profiles select `silk/native_start`; standalone wasm32 execution selects
`silk/wasm_start`. Both invoke the explicit application through `Intrinsic.application` and
export their own C `main`. The native entry receives process inputs; the Wasm entry takes no
arguments and supplies no host services or observer. Source owns integer/unit/Effect result
conversion. The installed modules are verified through explicit selection before changing
build defaults.

Object and library profiles select no startup by default. Selecting runtime `none` also
selects no default execution-storage component. The default executable compositions declare
their execution-storage bindings; custom and library compositions declare any support they
need explicitly. Foreign exports and retention remain the generic artifact roots. No source
module spelling grants compiler privilege. Shared Wasm hosts migrate to the source `main`
export, and link finalization retains declared exports instead of demanding `silk_main`.
The existing standalone WebAssembly output request defaults to the executable semantic
artifact form. Explicit loadable-module, archive and object profiles remain separate
from that execution default and do not select startup. Empty artifacts and artifacts
with only explicit retention are valid without an invocation or foreign export.

Wasm foreign declarations use LLVM's exact linker-symbol spelling (`\01` prefix),
consistently for imported/exported functions, data and C allocation helpers. The prefix
does not appear in the linked symbol. This preserves source-declared signatures even
for names LLVM otherwise treats as C language entry points: its WebAssembly backend
rewrites a plain zero-argument `main` into an argc/argv wrapper. No particular source
symbol or module is recognized by Silk. Conformance checks the linked export's arity
as well as its status or trap outcome.

## Goals / Non-Goals

**Goals:** Move each complete policy boundary to source, preserve established language semantics, and prove actual artifact closure on the designated targets.

**Non-Goals:** Static PIE, raw spawning, schedulers, networking, foreign TLS, arbitrary preemption, broad ABI extensions or a replacement ownership implementation. LTO remains rejected.

## Decisions

### Source combinator propagation

Sequential combinators propagate the original typed outcome directly, including its
diagnostic ownership; they do not convert a failure through `Result` merely to raise
it again. Error-mapping callbacks execute inside the ordinary selected recovery
scope so their new failure retains the selected cause. `mapBoth` composes success
mapping with that failure mapping. Acquired provider cleanup uses ordinary lexical
release before the propagated outcome leaves its helper. Finalization and retry preserve their specified outcome semantics as described below
and verified by the focused semantic and native/Wasm conformance cases.

Retry materializes only failures for which another attempt remains. It releases that
attempt's error before invoking the next attempt. The last permitted attempt executes
directly, so exhausting retries propagates its original outcome and diagnostic origin.
Earlier recovered attempts are intentionally discarded, as specified by retry's
final-failure contract.

Selected recovery invokes its handler once and runs its returned Effect once. Its
callable contract therefore accepts a `once Effect` result, admitting handlers
whose computation consumes captured state; reusable Effects can still satisfy that
consuming contract. This applies to the sealed catch primitive and both source
recovery wrappers. It introduces no additional execution or failure policy.

### Transparent finalization

`ensuring` must retain the complete protected outcome while running its finalizer.
Converting to `Result` and raising again cannot preserve the original diagnostic
owner. An ordinary selected catch would instead attach a new selected cause, which
also changes the promised outcome. A Drop wrapper cannot run a finalizer with an
arbitrary requirement row. The implementation therefore uses the minimal
target-neutral `Intrinsic.finalizeEffect(protected, finalizer)` composition seam.

Both inputs run at most once. After protected success or typed failure, all of its
local cleanup completes before the finalizer starts. The primitive holds the original
success or typed failure, including diagnostic ownership, until the infallible unit
finalizer completes; it then propagates that exact outcome. The finalizer runs in the
surrounding lexical observation scope, without treating the pending failure as a
selected recovery cause. Failure and requirement rows remain `! E ? R | S`.
Suspension in either input retains the same ownership. Dropping a parked composition
releases its held outcome and remaining captures once; a trap does not promise to run
the finalizer. Source `Effect.ensuring` owns the public wrapper and documentation.
Admission, all execution engines and cancellation/metadata verification accompany implementation.
Lowering keeps the pending outcome through ordinary catch projection and transparent propagation;
it creates no selected-recovery scope. Finite runtime choices select one exact capture environment
before entering the same invocation and suspension paths. The conformance ledger records the
completed target lanes and any remaining verification.

### Admission and sequencing

Use one ledger for the five tickets, but admit and verify each implementation boundary independently. Raw Linux is independent of hosted storage and reporting. Detailed storage-state and reporting ABI inventories must be completed before their lowering changes; the high-level requirements do not by themselves authorize an invented ABI. Process declaration admission likewise precedes replacing its complete protocol. No task is complete merely because its replacement source exists.

### Raw Linux composition

Use existing selected modules, `Intrinsic.application`, foreign exports and naked assembly. A source `_start` captures the incoming stack, aligns it for the C ABI, calls an explicitly retained source C export and traps if that export unexpectedly returns. The C export decodes the kernel stack and calls the ordinary application. No generated C entry participates. The raw profile is explicitly libc-none, static, non-PIE and initially synchronous; unsupported storage/report capabilities must fail before final link, without changing hosted support.

The x86-64 syscall number/result is rax; inputs use rdi/rsi/rdx/r10/r8/r9; rcx/r11/flags are clobbered. ARM64 uses x8 for the number and x0–x5 for arguments, with x0 as result. Source wrappers declare side effects and readwrite memory. Results in the unsigned encoding of -4095 through -1 are errors; no libc errno lookup occurs.

Stack decoding receives an unsafe kernel-provided readable word sequence. It validates argv's terminator, scans envp to its terminator, then reads auxiliary key/value pairs through AT_NULL. AT_PAGESZ must occur with a positive power-of-two value. The application receives argc, borrowed argv/envp and the validated page size. The process owns these inputs for its entire lifetime. Missing/invalid facts terminate with status 127 before application invocation. This is not an API for reading arbitrary untrusted memory.

Mapping allocation validates nonzero size and power-of-two alignment, checks arithmetic before addition, rounds the reservation to the runtime page size and uses anonymous private read/write mmap. Over-alignment reserves sufficient padding while retaining the complete mapping rather than splitting it. An affine owner stores base, mapped length, aligned usable address and requested length. Structured drop releases the complete mapping once; construction failure after mapping releases it before returning. Anonymous mappings supply zero-initialized bytes. Raw pointer access remains unsafe and cannot outlive the owner. No process-global allocation hint or cached host page size is used.

### Storage and reporting admission inventory

For JUL-129, inventory push/pop declarations and calls in NativeProgram, NativeFunction, NativeSuspension, NativeExecutionOperation and LlvmWasmRuntime, plus every retained allocator/helper import. Resolve state ownership across library instances, reentrant calls and execution handles before specifying the typed selected-component ABI. Frame shape, suspension state, failure propagation, cleanup edges and trap sites remain compiler responsibilities. Limits/accounting and acquisition/release implementations belong to selected source. Public suspension rows remain unchanged.

For JUL-130, inventory the existing entry invocation, all four host-input operations, report metadata production, report storage and output/exit operations. Retain semantic metadata; replace policy and state through an explicit source-instance contract. The payload must already be cleaned when unhandled typed failure reaches reporting. Capture report allocation/output failure without inventing trap recovery. The source runtime consumes the storage contract rather than introducing separate global state.

### Generic component catalog

Artifact composition carries explicit `components`, each naming a capability and
operation-to-source-declaration bindings. The catalog is independent of invocation,
loader entry and runtime module selection, so a library can select components
without acquiring executable startup. Configuration decoding rejects duplicate
capability or operation bindings and preserves selectors in cache identity.

Catalog entries alone do not load source modules or retain declarations. Actual
compiler capability demand must activate and type-check the required operations;
unused components add no source or native dependencies. This separates the generic
catalog from the execution-storage contract that will consume it. Binding a source
name grants no compiler privilege. The storage lifetime/ABI admission remains
required before adding its demand activation and lowering.

### Process admission

Compare the existing fork/exec protocol with the selected platforms' spawn APIs using actual pinned headers. Choose one complete protocol only after verifying its records/signatures, descriptor inheritance and cwd availability. Existing integer variadics can support open/fcntl if chosen. Model every acquired endpoint and child as an affine obligation. Captures belong to each invocation and drain both ready streams; error exits close endpoints and resolve the child without replacing the primary failure. Blocking foreign calls do not imply arbitrary cancellation points.

### Evidence and authorities

Reuse LLVM/Clang/LLD 22.1.8, the pinned GNU containers and linux-libc-dev 6.1.180-1 from `native-assembly-entry-contracts/{supplies,assembly-supplies}.json`; extend with independently compiled UAPI constants and ELF checks. Reuse the existing Darwin SDK/deployment and GNU catalog evidence for hosted declarations, extending it only after header inspection. Required lanes fail on missing inputs and skipped cases.

The local Zig checkout is exactly e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa; the Rust checkout is c33d8f3b5a50b56466998e8c5ed8a077d2caed84. Initial inspection covered Zig's x86-64/ARM64 syscall0–6, PageAllocator and stack-decoding startup, plus Rust's runtime and naked-function fixture. Zig supplies a useful raw composition analogue; Silk uses its own affine ownership and checked assembly constraints. Rust std's hosted initialization is not an equivalent raw runtime. Further slice-specific implementation/test comparisons and exact authority hashes remain admission tasks, not completed conformance claims.

## Risks / Trade-offs

- Hidden support can appear after legalization: inspect actual program/provider objects and final ELF, not only source imports.
- A state ABI can accidentally share accounting across instances: complete the lifecycle inventory before choosing or lowering it.
- Blocking capture failures can strand a child: explicit acquisition and reaping ownership must cover every recoverable boundary.
- A source file can compile while its ABI is wrong: independent C/UAPI fixtures and actual debug/optimized objects remain mandatory.

## Migration Plan

Complete admission per slice, implement and remove its obsolete path together, migrate every affected consumer, and verify it before checking off its ticket tasks. Maintain the final audit ledger during implementation. Do not archive or claim core closure until all replacements and designated checks are complete.

### Shared imported data symbols

The hosted source runtime and application code may independently read the same C data symbol (for example GNU `environ`). Planning accepts repeated imports only when their classified C value types agree, emits one LLVM global, and records one artifact data import. It continues to reject incompatible value types, function/data collisions, duplicate exports, and import/export collisions before emission. Source names do not create separate C globals; the classified pointer contract must still agree.
