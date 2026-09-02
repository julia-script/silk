## MODIFIED Requirements

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
