# bootstrap-native-toolchain Specification

## Purpose

Pinned-Clang process orchestration between deterministic bitcode and a runnable binary: object
emission under fixed optimization profiles, build-scope-owned path-backed intermediates, the
`NativeLinker` service with its `ClangLinker` implementation, and the minimal C runtime shim that
reaches a closed native entry.

## Requirements

### Requirement: A pinned Clang emits the target object under fixed profiles

Object emission SHALL invoke the caller-pinned external Clang with `-c`, the canonical target from
the compiler-selected MIR plan, and structured arguments — never a shell command string — over the
backend's bitcode. Optimization profiles are fixed: debug is `-O0` with debug metadata, release is
`-O2` without debug metadata, and release-with-debug is `-O2` with line information. There is no
configurable pass pipeline, and a process failure or bitcode/target mismatch SHALL surface as data
retaining the exact command, arguments, exit status, and process output rather than throwing.

#### Scenario: Emit a release object

- **WHEN** the nested-call program's bitcode is emitted with the release profile through the pinned Clang
- **THEN** a non-empty relocatable object for the compiler-selected target exists at the scope-owned path and the outcome records the exact command and arguments issued

#### Scenario: Surface a failed process as data

- **WHEN** the pinned Clang path does not exist or the process exits non-zero
- **THEN** the outcome is a failure value carrying the command, structured arguments, status, and retained output

#### Scenario: Plan fixed profile arguments

- **WHEN** the three profiles' commands are planned for the same target-aware input
- **THEN** every plan names the same canonical target while debug plans `-O0` with `-g`, release plans `-O2` without `-g`, and release-with-debug plans `-O2` with line information — and nothing else varies

### Requirement: Intermediates are build-scope-owned path artifacts

Bitcode and object intermediates SHALL be owned, path-backed artifacts tied to a named build
scope. Leaving the scope SHALL remove them after success or failure alike; retaining them
requires an explicit save-temps promotion that copies the artifact to a caller-chosen durable
destination. Large process outputs MUST NOT be read into memory merely to be written again.

#### Scenario: Remove intermediates at scope exit

- **WHEN** a build scope emits bitcode and object files and then exits, successfully or not
- **THEN** the scope's directory and every unpromoted artifact are removed

#### Scenario: Promote an artifact explicitly

- **WHEN** a caller promotes an object artifact to a durable destination before scope exit
- **THEN** the promoted copy survives scope removal at the requested path

### Requirement: The NativeLinker service drives the pinned Clang driver

The `NativeLinker` service SHALL validate that its inputs exist and match the compiler-selected
canonical target, combine the program object with runtime objects built for that target, any
request-supplied native object files, and the approved system libraries plus any request-supplied
library names, invoke the pinned Clang driver with structured target arguments, and write the
executable to the requested durable destination. Library names SHALL be passed as structured
`-l<name>` arguments only; the service SHALL NOT accept arbitrary linker flags. On failure the
outcome SHALL retain process output, exit status, and command provenance as data.

#### Scenario: Link a runnable executable

- **WHEN** the `ClangLinker` links the program object with a runtime shim compiled for the same canonical target
- **THEN** an executable exists at the requested destination and running it exits with the program's `i32` result

#### Scenario: Link a request-supplied C object

- **WHEN** the request supplies a C object defining a foreign symbol the program reaches
- **THEN** the planned command lists that object after the program and shim objects and the executable resolves the symbol

#### Scenario: Pass a request-supplied library

- **WHEN** the request supplies the library name `c`
- **THEN** the planned command contains `-lc` after the object inputs and before the destination

#### Scenario: Reject a missing input as data

- **WHEN** a linker input path does not exist
- **THEN** the outcome is a failure value naming the missing input without invoking the driver

#### Scenario: Reject a target mismatch

- **WHEN** a program object and runtime object name different canonical targets
- **THEN** linking returns a target-compatibility failure before invoking Clang

### Requirement: The minimal runtime shim reaches a closed native entry

The toolchain SHALL generate the slice's minimal C runtime shim and compile it with the pinned
Clang. Its private, compiler-versioned scalar ABI SHALL call the explicit zero-parameter
`silk_main -> i32`. For an ordinary entry, the shim SHALL return that result unchanged as the
process exit status. For an effectful entry, `silk_main` SHALL return `0` or a normalized one-based
failure tag; the shim SHALL map success to status `0`, map a valid failure tag through its
compiler-provided canonical report table to one standard-error line and status `1`, and map an
incomplete standard-error write or invalid tag to operational status `2`. The shim is not
user-facing FFI, and its generated source is owned by the compiler.

#### Scenario: Compile and honor the ordinary shim ABI

- **WHEN** the shim is compiled and linked with an ordinary program whose `silk_main` returns `42`
- **THEN** the resulting executable exits with status `42`

#### Scenario: Report and normalize an effect failure

- **WHEN** an effectful program's `silk_main` returns the tag for `app.SomeError`
- **THEN** the shim writes `Error: app.SomeError\n` to standard error and exits with status `1`

#### Scenario: Reject an invalid effect failure tag

- **WHEN** an effectful program's `silk_main` returns a tag absent from its generated report table
- **THEN** the shim exits with operational status `2`

### Requirement: The private platform boundary supports system allocation

The compiler-versioned native runtime shim SHALL expose only the aligned system acquisition and
infallible release operations needed by the standard-library `SystemAllocator` conformance. A valid
layout SHALL produce either an opaque successful block identity or an allocation-free exhaustion
status. The boundary SHALL preserve requested size and alignment for release, support valid
zero-sized and over-aligned layouts, and expose no public `free`, resize, zero-fill, allocator-kind,
or stable external ABI promise.

#### Scenario: Translate native exhaustion

- **WHEN** the private aligned acquisition boundary cannot satisfy a valid layout
- **THEN** native execution returns the status used for typed `OutOfMemoryError` and creates no releasable block

#### Scenario: Release an over-aligned block

- **WHEN** native execution drops a successful over-aligned Allocation
- **THEN** its captured reclaim ticket invokes the matching private release exactly once with no ambient provider lookup

### Requirement: The pinned Clang finalizes LLVM WebAssembly

The external toolchain boundary SHALL finalize LLVM bitcode planned for `wasm32-unknown-unknown` into a standalone WebAssembly module using structured pinned-Clang arguments, no shell command string, no native runtime shim, no implicit host libraries, and an exported zero-argument `silk_main` entry returning `i32`. The resulting module SHALL be atomically committed to the requested `.wasm` destination. Process failure or incompatible output SHALL return typed data retaining the command, arguments, exit status, and process output.

#### Scenario: Finalize a standalone Wasm module

- **WHEN** compatible LLVM bitcode is finalized for `wasm32-unknown-unknown`
- **THEN** the destination is an instantiable WebAssembly module exporting `silk_main`

#### Scenario: Avoid the native shim

- **WHEN** LLVM WebAssembly finalization is planned
- **THEN** neither the native C `main` shim nor host system libraries are compiled or linked

#### Scenario: Surface Clang failure as data

- **WHEN** pinned Clang cannot finalize the Wasm module
- **THEN** the outcome contains the exact structured invocation and retained failure output without a partial destination

### Requirement: Direct WebAssembly finalization bypasses the external toolchain

When the selected backend artifact already contains validated final WebAssembly bytes, durable finalization SHALL atomically write those bytes and MUST NOT invoke Clang or any linker.

#### Scenario: Commit direct Wasm bytes

- **WHEN** backend `wasm` returns a valid final module artifact
- **THEN** those bytes are committed at the requested destination with zero external-tool invocations

### Requirement: Finalization preserves stream requirements

Native finalization SHALL connect an explicit process-stream provider. WebAssembly finalization SHALL retain and expose the required host import in inspection data. Neither path SHALL add an implicit Logger or console dependency.

#### Scenario: Run native output

- **WHEN** a native program is finalized with the process provider
- **THEN** running it emits the exact requested bytes to the selected destination

#### Scenario: Inspect a Wasm requirement

- **WHEN** a Wasm program requires `StandardStreams`
- **THEN** finalization preserves the import required for instantiation

### Requirement: The default artifact cache persists to a configured directory

When the `SILK_NATIVE_CACHE_DIR` environment variable names a directory, the toolchain's default
artifact cache SHALL persist finalized native and WebAssembly artifacts in that directory, keyed by
the content of the compilation request: artifact kind, target triple, profile, Clang identity,
runtime shim, input bitcode, the bytes of every request-supplied native object, and the ordered
request-supplied library names. A request whose key matches a stored artifact SHALL reuse it
without invoking the external toolchain. When the variable is unset, the default cache SHALL retain
its process-local behavior unchanged. A corrupted or missing cache entry SHALL cause recompilation,
never a failed or incorrect build.

#### Scenario: A second process reuses a cached artifact

- **WHEN** two processes compile an identical request with `SILK_NATIVE_CACHE_DIR` set to the same directory
- **THEN** the second process produces a byte-identical artifact without invoking Clang

#### Scenario: A changed input misses the cache

- **WHEN** the bitcode, profile, target, shim, Clang identity, a native object's bytes, or the library list of a request differs from every stored entry
- **THEN** the toolchain compiles the request through Clang and stores the new artifact under its own key

#### Scenario: The variable is unset

- **WHEN** `SILK_NATIVE_CACHE_DIR` is not set
- **THEN** the default cache remains process-local and no artifact is written outside the build's own scope

### Requirement: Native toolchain failures yield typed errors

Every expected native-toolchain failure (spawn, write, rename, temp-dir creation) SHALL surface in
the typed error channel with operation and stage provenance and a preserved cause. An fs or
subprocess failure SHALL NOT escape an Effect generator as a defect.

#### Scenario: A spawn failure is a typed failure

- **WHEN** the pinned Clang exits with a non-zero status or cannot be spawned
- **THEN** the operation yields a typed toolchain failure carrying the command and stage, not a thrown error

#### Scenario: A storage failure carries the failing stage

- **WHEN** an intermediate write fails
- **THEN** the yielded failure names the stage that failed and preserves the underlying cause

### Requirement: Artifact and cache commits are atomic and always clean up

Every durable commit SHALL stage to a temporary sibling and rename atomically, and SHALL remove the
temporary on any failure so no stale temporary file survives.

#### Scenario: A failed rename leaves no temp sibling

- **WHEN** a staged write is followed by a failing rename
- **THEN** the temporary file is removed and the destination is left untouched

### Requirement: The compiler root barrel stays browser-safe

Importing the compiler package root SHALL NOT pull Node built-ins. Host target detection
(platform/arch) SHALL be resolved through a Node-only boundary reached by deep import, not from
modules re-exported by the root barrel.

#### Scenario: Importing the root barrel does not load node:os

- **WHEN** a consumer imports the compiler package root in a browser bundle
- **THEN** no Node built-in module is imported transitively

### Requirement: The driver does not read artifacts back from the filesystem

Artifact bytes produced by a finalizer SHALL be returned in-memory to the caller; the driver SHALL
NOT re-read them with a synchronous filesystem call to seed the cache.

#### Scenario: Finalized bytes are returned in memory

- **WHEN** a finalizer produces an artifact
- **THEN** its bytes are available to the cache without a second read from disk
