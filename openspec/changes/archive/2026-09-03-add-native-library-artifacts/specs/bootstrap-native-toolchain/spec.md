## RENAMED Requirements

- FROM: `### Requirement: The NativeLinker service drives the pinned Clang driver`
- TO: `### Requirement: The native finalizer uses typed link inputs and target tool plans`
- FROM: `### Requirement: The default artifact cache persists to a configured directory`
- TO: `### Requirement: The default artifact cache covers native artifact and link structure`

## MODIFIED Requirements

### Requirement: The native finalizer uses typed link inputs and target tool plans

Native finalization SHALL accept an ordered immutable union of object paths, static-archive paths,
named libraries with explicit static or dynamic mode, search paths, and frameworks. It SHALL
translate those values to structured process arguments without accepting raw linker flags. An
executable or shared-library request SHALL use the target Clang driver; a static-library request
SHALL use deterministic archive mode. Inputs or combinations unsupported by the selected target or
artifact kind SHALL fail as typed toolchain data before a process is invoked.

#### Scenario: Link a runnable executable

- **WHEN** the native finalizer receives an executable request, the program object, its runtime object, and valid structured link inputs
- **THEN** it invokes target Clang with structured arguments and atomically commits an executable whose process status is the program result

#### Scenario: Link a shared library

- **WHEN** the native finalizer receives a shared-library request for a supported native target
- **THEN** it invokes target Clang in that target's shared-library mode and atomically commits a loadable shared-library artifact

#### Scenario: Create a deterministic static library

- **WHEN** the native finalizer receives the same static-library request and object bytes twice
- **THEN** it invokes the pinned LLVM archive tool with deterministic `rcsD` semantics and produces byte-identical archives

#### Scenario: Preserve structured input order

- **WHEN** a request supplies a search path, a named dynamic library, an object, a static archive, and a framework in that order
- **THEN** the planned Clang arguments encode the supported inputs in the same order with no shell parsing or arbitrary flag channel

#### Scenario: Reject an unsupported link input

- **WHEN** a framework is requested for a non-Apple target or a non-object input is requested for a static archive
- **THEN** finalization returns typed unsupported-input data naming the artifact kind, target, and input without invoking a tool

#### Scenario: Reject a missing path input

- **WHEN** an object or static-archive input path does not exist
- **THEN** finalization returns a typed failure naming the missing input without invoking the tool

#### Scenario: Link a request-supplied C object

- **WHEN** the request supplies a C object defining a foreign symbol the program reaches
- **THEN** the planned command lists that object after the program and runtime objects and the artifact resolves the symbol

#### Scenario: Pass a request-supplied library

- **WHEN** the request supplies the dynamic library name `c`
- **THEN** the planned command contains `-lc` in the input's ordered position

#### Scenario: Reject a missing input as data

- **WHEN** a path-backed native link input does not exist
- **THEN** the outcome is a failure value naming the missing input without invoking the selected tool

#### Scenario: Reject a target mismatch

- **WHEN** a program object and runtime object name different canonical targets
- **THEN** finalization returns a target-compatibility failure before invoking a tool

## ADDED Requirements

### Requirement: Native libraries expose only explicit C exports

Shared libraries SHALL make only explicit `export "C"` thunk symbols default-visible. Compiler
implementation functions, generated entry adapters, helpers, and runtime-support definitions MUST
be hidden or local in the produced library. Static libraries SHALL retain the same public thunk
names and MUST NOT promote a compiler-private symbol into the public ABI.

#### Scenario: Inspect a shared library symbol table

- **WHEN** a shared library containing one explicit C export and reachable internal/runtime support is inspected with the platform symbol dumper
- **THEN** the export thunk is the only Silk-defined default-visible symbol and no compiler implementation or runtime-support name is exported

#### Scenario: Link a C consumer

- **WHEN** a separately compiled C program links and calls an explicit exported thunk from the shared library
- **THEN** the call succeeds with the declared C ABI while no compiler-private symbol is required by the consumer

## MODIFIED Requirements

### Requirement: The default artifact cache covers native artifact and link structure

When the `SILK_NATIVE_CACHE_DIR` environment variable names a directory, the default artifact
cache SHALL key finalized native and WebAssembly artifacts by artifact kind, target triple,
optimization profile, selected tool identities, runtime source, input bitcode, the bytes of every
path-backed link input, and the canonical ordered encoding of every other structured link input. A
matching request SHALL reuse the stored artifact without invoking an external tool. A corrupted or
missing entry SHALL cause recompilation, never an incorrect build.

#### Scenario: A second process reuses a cached artifact

- **WHEN** two processes compile an identical request with `SILK_NATIVE_CACHE_DIR` set to the same directory
- **THEN** the second process produces a byte-identical artifact without invoking the selected external tool

#### Scenario: A changed input misses the cache

- **WHEN** an artifact kind, input path's bytes, library mode, library name, search path, or framework differs from a cached request
- **THEN** the finalizer invokes the required tool and stores the artifact under a distinct key

#### Scenario: The variable is unset

- **WHEN** `SILK_NATIVE_CACHE_DIR` is not set
- **THEN** the default cache remains process-local and no cache artifact is written outside the build scope
