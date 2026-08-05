## MODIFIED Requirements

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

### Requirement: The NativeLinker service drives the pinned Clang driver

The `NativeLinker` service SHALL validate that its inputs exist and match the compiler-selected
canonical target, combine the program object with runtime objects built for that target and
approved system libraries, invoke the pinned Clang driver with structured target arguments, and
write the executable to the requested durable destination. On failure the outcome SHALL retain
process output, exit status, and command provenance as data.

#### Scenario: Link a runnable executable

- **WHEN** the `ClangLinker` links the program object with a runtime shim compiled for the same canonical target
- **THEN** an executable exists at the requested destination and running it exits with the program's `I32` result

#### Scenario: Reject a missing input as data

- **WHEN** a linker input path does not exist
- **THEN** the outcome is a failure value naming the missing input without invoking the driver

#### Scenario: Reject a target mismatch

- **WHEN** a program object and runtime object name different canonical targets
- **THEN** linking returns a target-compatibility failure before invoking Clang
