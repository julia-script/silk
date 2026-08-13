# bootstrap-entry-termination Specification

## Purpose

Define how a closed Silk executable runs an effectful user entry, reports an unhandled typed
failure, releases its owned payload, and converts the closed outcome to a platform status.

## Requirements

### Requirement: Executables accept an effectful unit entry

A root module SHALL be executable when it declares exactly one public, non-generic,
zero-parameter `effect fn main() -> () ! E` whose requirement row is empty and whose failure row
is closed. Calling the entry SHALL construct its lazy Effect, and the generated host adapter SHALL
run exactly that Effect exactly once. The existing public, non-generic, zero-parameter ordinary
`main() -> i32` form SHALL retain its direct exit-status behavior.

#### Scenario: Complete an effectful entry

- **WHEN** `pub effect fn main() -> ()` returns `()`
- **THEN** the generated adapter runs the Effect once and the executable exits with status `0`

#### Scenario: Preserve an ordinary status entry

- **WHEN** `pub fn main() -> i32` returns `42`
- **THEN** the executable exits with status `42` without applying effectful termination semantics

#### Scenario: Reject unresolved entry requirements

- **WHEN** an effectful `main` retains any capability requirement
- **THEN** entry discovery reports an unavailable entry and no runtime artifact is produced

### Requirement: Entry failures require explicit reportability

The compiler SHALL expose a compiler-sealed marker capability named `Report`. Every nominal member
of an effectful entry's failure row MUST have exactly one valid `Report` conformance. A valid
bootstrap conformance SHALL be an operation-free marker declaration. Missing, malformed, or
ambiguous reportability SHALL make the entry unavailable before MIR lowering.

#### Scenario: Accept a reportable failure

- **WHEN** `SomeError` has a valid `impl Report for SomeError {}` and effectful `main` declares `! SomeError`
- **THEN** entry discovery retains `SomeError` as an ordered reportable failure

#### Scenario: Reject an unreportable failure

- **WHEN** effectful `main` declares `! SomeError` without a valid `Report` conformance
- **THEN** entry discovery reports that the entry has an unreportable failure and records no instances

### Requirement: The host adapter closes typed entry outcomes

The generated host adapter SHALL branch on the effect entry's explicit typed outcome. Success SHALL
become status `0`. An unhandled typed failure SHALL select its normalized one-based failure tag,
release the complete owned failure payload through its compiler-planned cleanup, and produce a
closed failure termination. No typed failure or requirement row MAY cross the machine entry ABI.

#### Scenario: Close one failure

- **WHEN** effectful `main` fails with its first normalized reportable failure member
- **THEN** the adapter releases that payload and produces failure termination tag `1`

#### Scenario: Close a later failure deterministically

- **WHEN** effectful `main` fails with normalized failure member `n`
- **THEN** repeated builds and runs produce failure termination tag `n` and the same cleanup behavior

#### Scenario: Run a failure Drop hook

- **WHEN** the unhandled failure payload owns a type with a reachable Drop hook
- **THEN** the adapter invokes that hook exactly once before returning across the machine boundary

### Requirement: Native failures are reported and normalized

For a native executable, the compiler-owned runtime shim SHALL map an effect failure termination to
the exact colorless UTF-8 line `Error: <canonical-failure-identity>\n` on standard error and process
status `1`. The canonical identity SHALL be selected from compiler-provided ordered metadata, not
runtime reflection. A complete failed write SHALL produce operational status `2`. The report MUST
NOT include uninitialized padding, raw addresses, or an implicit structural dump of the payload.

#### Scenario: Report an unhandled failure

- **WHEN** effectful `main` fails with reportable `app.SomeError`
- **THEN** the native executable writes `Error: app.SomeError\n` to standard error and exits with status `1`

#### Scenario: Keep failure reports deterministic

- **WHEN** equivalent failed programs are compiled and run repeatedly
- **THEN** their report bytes and exit status are identical

#### Scenario: Classify a broken standard error write

- **WHEN** the shim cannot write the complete failure report to standard error
- **THEN** it exits with operational status `2`

### Requirement: Standalone WebAssembly retains host-reportable termination

A standalone WebAssembly artifact SHALL keep its import-free boundary. Its exported `silk_main`
SHALL return `0` for effect success or the normalized one-based failure tag for an unhandled typed
failure, and the backend artifact SHALL retain the matching ordered canonical failure identities so
a host can render the same report. The module SHALL NOT invent a standard-error import.

#### Scenario: Return a WebAssembly failure tag

- **WHEN** direct WebAssembly execution reaches the second normalized entry failure
- **THEN** `silk_main` returns `2` and the artifact's second report identity names that failure

### Requirement: Traps bypass typed entry termination

Arithmetic traps, bounds traps, compiler-generated impossible-state traps, and violated unsafe
contracts SHALL remain abnormal termination. The host adapter MUST NOT render them as reportable
typed failures or claim that typed-failure cleanup completed.

#### Scenario: Bypass reporting for a trap

- **WHEN** effectful `main` traps while running
- **THEN** execution terminates abnormally without a `Report` failure line or typed-failure status

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
