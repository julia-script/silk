# bootstrap-entry-termination Specification

## Purpose

Define how a closed Silk executable runs its selected user entry, represents success, unhandled
typed failure, and fatal trap, and adapts that target-neutral outcome to each host boundary.

## Requirements

### Requirement: Entrypoints use the confirmed public shapes and statuses

The compiler SHALL accept exactly public, non-generic, zero-parameter ordinary `main() -> ()`,
ordinary `main() -> i32`, and effectful `main() -> () ! E ? never`. It SHALL map ordinary unit and
Effect success to status zero, every unhandled typed failure to status one, and only ordinary
`i32` success to its returned custom status. A private, invalidly shaped, or open entry SHALL be
rejected before lowering with its exact entry reason.

#### Scenario: Run an ordinary unit entry

- **WHEN** a public ordinary `main` explicitly returns `()`
- **THEN** the program is a valid entry and terminates successfully with status zero

#### Scenario: Preserve an ordinary custom status

- **WHEN** a public ordinary `main() -> i32` returns one concrete status
- **THEN** the program terminates with that exact status without applying Effect termination policy

#### Scenario: Run an effect entry exactly once

- **WHEN** a public effectful `main` succeeds with `()` and retains no requirements
- **THEN** the compiler constructs and runs that Effect exactly once and terminates with status zero

#### Scenario: Diagnose a private entry accurately

- **WHEN** a correctly typed `main` is not public
- **THEN** entry discovery reports the visibility error rather than claiming its return type is unresolved

#### Scenario: Reject unresolved entry dependencies

- **WHEN** an effectful `main` retains one or more requirement keys
- **THEN** compilation lists every unresolved dependency and does not begin execution

### Requirement: Termination is structured target-neutral data

Success, unhandled typed failure, and fatal trap SHALL produce one target-neutral outcome carrying
classification, public status, failure identity or trap reason, source provenance, stable logical
call path, and causal recovery history where present. Any concrete detached owned failure value
SHALL be entry-eligible without marker conformance. Ordinary propagation MUST NOT duplicate a
cause. Typed-failure propagation SHALL complete ordinary cleanup, including cleaning the terminal
payload exactly once at entry; a trap remains outside typed recovery and promises no cleanup.
Standalone adapters MAY render this data, while embedding hosts SHALL receive data without ambient
output.

#### Scenario: Close an arbitrary concrete failure

- **WHEN** effectful `main` fails with a concrete detached owned value that implements no marker interface
- **THEN** entry closure cleans its payload exactly once and returns an unhandled-failure outcome with status one and its canonical identity

#### Scenario: Retain causal recovery history

- **WHEN** a selected recovery handler fails while handling an earlier typed failure
- **THEN** the new failure is primary and the earlier failure remains one recovered cause without propagation duplicates

#### Scenario: Preserve a failure trace through suspension

- **WHEN** an unhandled typed failure crosses suspended logical invocations in an optimized build
- **THEN** evaluator, native, and Wasm termination metadata retain the stable logical path rather than compiler driver or coroutine frames

#### Scenario: Keep traps outside typed recovery

- **WHEN** execution reaches a fatal trap
- **THEN** the outcome is classified as a trap, `Effect.catch` cannot recover it, and no successful source cleanup is claimed

### Requirement: Generated adapters are target-specific and pay for use

The compiler SHALL derive each private target adapter and runtime linkage from the reachable entry,
failure, host-input, stream, and suspension inventory. A native standalone adapter SHALL normalize
every recognized unhandled typed failure to status one and render its canonical identity to the
diagnostic stream; a failed complete render SHALL use operational status two. Direct WebAssembly
SHALL remain import-free and return the private normalized tag paired with the artifact's structured
termination metadata. Process arguments SHALL be captured only when reachable host-input operations
request them. No source-visible console, scheduler, allocator, dependency container, or provider
SHALL become ambient through entry adaptation.

#### Scenario: Keep a trivial adapter minimal

- **WHEN** a closed trivial entry reaches no reporting output, host input, suspension, or runtime service
- **THEN** its artifact contains no console, command-line, scheduler, allocator, provider-container, or unrelated adapter machinery

#### Scenario: Render a standalone typed failure

- **WHEN** a native effect entry fails with canonical identity `app.SomeError`
- **THEN** the adapter writes `Error: app.SomeError\n` to its diagnostic stream and exits with status one

#### Scenario: Classify a broken diagnostic write

- **WHEN** the native adapter cannot complete the failure report write
- **THEN** it exits with operational status two without changing the original typed-failure data

#### Scenario: Keep direct WebAssembly import-free

- **WHEN** a closed effect entry is emitted directly to WebAssembly
- **THEN** `silk_main` returns its private closed tag, the artifact retains matching structured termination metadata, and no standard-error import is added

#### Scenario: Capture command-line state only on demand

- **WHEN** reachable code uses host-input argument operations
- **THEN** the native adapter receives and stores the process command line without changing Silk `main`'s zero-parameter contract

### Requirement: The native entry receives the process command line

The compiler-owned native runtime shim's entry point SHALL receive the process argument count and
argument vector and retain them for the host-input runtime before running the user entry. Silk `main`
SHALL keep its zero-parameter, empty-requirement-row shape in both its effectful and its ordinary
status form: a program SHALL reach its arguments through a service rather than through the entry
signature. Retaining the command line MUST NOT change any termination status, report byte, or
cleanup behavior, and MUST NOT make the entry unavailable for a program that reads no host input.

#### Scenario: Keep a zero-parameter entry

- **WHEN** a program reads its command line through the host-input service
- **THEN** its `main` still declares no parameters and no requirement row, and entry discovery accepts it unchanged

#### Scenario: Preserve every status with arguments present

- **WHEN** a program that reads no host input is run with arguments
- **THEN** it exits `0` on success, `1` with the same report bytes on a reported typed failure, and `2` on a failed standard-error write

#### Scenario: Leave standalone WebAssembly unchanged

- **WHEN** a standalone WebAssembly artifact is produced
- **THEN** it keeps its import-free boundary and its exported `silk_main` contract, with no process-input import and no entry parameters
