## MODIFIED Requirements

### Requirement: The native finalizer uses typed link inputs and target tool plans

Native finalization SHALL accept an ordered immutable union of object paths, static-archive paths,
named libraries with explicit static or dynamic mode, search paths, and frameworks. It SHALL
translate those values to structured process arguments without accepting raw linker flags. An
object, static-archive, or search-path input SHALL carry an absolute path before planning so its
spelling cannot be interpreted as a tool option. An
executable or shared-library request SHALL resolve the target Clang driver plan and execute its frozen concrete linker command; a static-library request
SHALL use deterministic archive mode. Inputs or combinations unsupported by the selected target or
artifact kind SHALL fail as typed toolchain data before a process is invoked.

#### Scenario: Link a runnable executable

- **WHEN** the native finalizer receives an executable request, the program object, its runtime object, and valid structured link inputs
- **THEN** it executes the resolved linker with structured arguments and atomically commits an executable whose process status is the program result

#### Scenario: Link a shared library

- **WHEN** the native finalizer receives a shared-library request for a supported native target
- **THEN** it executes the resolved linker in that target's shared-library mode and atomically commits a loadable shared-library artifact

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
- **THEN** the resolved command contains the concrete selected libc input in the input's ordered position

#### Scenario: Reject a missing input as data

- **WHEN** a path-backed native link input does not exist
- **THEN** the outcome is a failure value naming the missing input without invoking the selected tool

#### Scenario: Reject a target mismatch

- **WHEN** a program object and runtime object name different canonical targets
- **THEN** finalization returns a target-compatibility failure before invoking a tool

The resolved physical plan SHALL follow the platform-supplies contract, including full recursive
input closure, frozen environment, compatibility validation, and selected linker content identity.

### Requirement: The default artifact cache covers native artifact and link structure

When the `SILK_NATIVE_CACHE_DIR` environment variable names a directory, the default artifact
cache SHALL store each finalized native and WebAssembly artifact in a versioned, checksummed
envelope and key native outputs by artifact kind, target triple, resolved entry policy, selected
final tool bytes, emitted program/runtime object bytes, the complete ordered resolved input
closure, and any destination-derived identity embedded in the artifact. Optimization and runtime
source participate through the actual emitted objects; physical supply paths SHALL NOT leak back
into logical semantic or object identity. Native reuse and storage SHALL require complete resolved physical input accounting under the permanent cache-admission rule; named inputs and transitive references SHALL be hashed by selected bytes, not spelling alone. A matching entry SHALL be reused only
after its container, artifact kind, and target validate without invoking final linking. A
corrupted, unauthenticated, or missing entry SHALL cause recompilation, never an incorrect build.

#### Scenario: A second process reuses a cached artifact

- **WHEN** two processes compile an identical request with `SILK_NATIVE_CACHE_DIR` set to the same directory
- **THEN** the second process produces a byte-identical artifact without invoking final linking after complete supply validation

#### Scenario: A changed input misses the cache

- **WHEN** an artifact kind, input path's bytes, library mode, library name, search path, framework, or embedded shared-library identity differs from a cached request
- **THEN** the finalizer invokes the required tool and stores the artifact under a distinct key

#### Scenario: The variable is unset

- **WHEN** `SILK_NATIVE_CACHE_DIR` is not set
- **THEN** the default cache remains process-local and no cache artifact is written outside the build scope

C runtime-object cache identities SHALL account for the frozen compiler bytes and actual consumed
preprocessed source/header subset. Unconsumed headers SHALL not affect an object identity.

#### Scenario: Runtime header invalidation

- **WHEN** a runtime C source is unchanged but a consumed header changes
- **THEN** the runtime object cache misses for the new frozen translation unit
