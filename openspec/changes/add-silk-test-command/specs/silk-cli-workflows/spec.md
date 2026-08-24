## ADDED Requirements

### Requirement: Silk test runs one evaluator suite

The root `silk` command SHALL expose `test` as a project-oriented workflow accepting shared project
discovery and manifest selection plus zero or more positional raw filter arguments. Passing
`--standard-library` SHALL instead bypass project discovery and manifest loading, reject
`--manifest-path`, and select the deterministic toolchain catalog. It SHALL compose
the selected distinct zero-parameter runner root with the user or standard-library test-root union,
reject an invalid or unavailable test compilation before execution, and run the runner exactly once
through the evaluator for the project's ordinary host target. The initial command MUST NOT expose an
engine selector, backend selector, target selector or matrix, profile or release selector, watch
mode, or special native or WebAssembly execution path. Manifest build backend, targets, and profile
SHALL NOT affect testing. `--` SHALL end option parsing and cause every following argument to be
treated as raw filter bytes.

#### Scenario: Run the default user suite

- **WHEN** a valid project with no `[test]` table invokes `silk test` without filters
- **THEN** the command evaluates the standard runner once over the complete package-root inventory for the ordinary host target

#### Scenario: Run the standard-library suite

- **WHEN** `silk test --standard-library` is invoked without a manifest selector
- **THEN** the command evaluates the shipped standard runner once over exactly the cataloged standard-library test roots without discovering a user project

#### Scenario: Run a custom runner

- **WHEN** the manifest selects a valid custom runner root
- **THEN** `silk test` evaluates that runner once with the same inventory and raw host-input contract

#### Scenario: Display the test command

- **WHEN** a user requests root help and then `silk test --help`
- **THEN** root help retains every existing subcommand, includes test with its purpose, and excludes compile, while test help exposes variadic filters, `--manifest-path`, and `--standard-library` but none of the forbidden execution controls

#### Scenario: Pass an option-looking filter

- **WHEN** a user invokes `silk test -- --profile`
- **THEN** the bytes spelling `--profile` are forwarded as one filter rather than parsed as a profile option

#### Scenario: Reject an invalid test compilation

- **WHEN** a root is unavailable or a marked declaration fails eligibility
- **THEN** the command reports the ordinary project or source diagnostic and does not invoke a partial suite

#### Scenario: Reject an unavailable configured entry

- **WHEN** a configured test root or custom runner file is absent or unreadable
- **THEN** the command reports the storage or project failure, exits 2, and does not invoke a partial suite

#### Scenario: Reject an absent import as source damage

- **WHEN** an available configured root has a statically absent transitive import and no operational failure occurs
- **THEN** the command reports source rejection, exits 1, and invokes no runner

#### Scenario: Keep traps fatal

- **WHEN** a selected test traps during evaluator execution
- **THEN** the command follows the existing fatal evaluator termination path rather than converting the trap to a failed case

### Requirement: Test filters use the ordinary raw host-input boundary

The command SHALL seed the evaluator's existing low-level host-input adapter with its admitted
platform-derived program-name bytes at argument zero, followed by each positional filter's unchanged
raw bytes and original order. The index-zero bytes SHALL be forwarded unchanged under the existing
HostInput convention but SHALL have no new stable cross-platform spelling. It MUST NOT parse,
decode, normalize, or apply filters in the compiler or CLI workflow. The zero-parameter runner SHALL
construct ordinary `OsHostInput` and `Allocator` providers and provide them lexically while reading
arguments one and later. A custom runner SHALL be free to interpret or ignore those bytes.

#### Scenario: Forward several raw filters

- **WHEN** `silk test` receives two positional filters
- **THEN** host input exposes the unchanged platform program name at index zero and the two unchanged byte sequences at indices one and two in command order

#### Scenario: Preserve invalid UTF-8

- **WHEN** the process supplies a filter containing bytes that are not valid UTF-8
- **THEN** the adapter retains those exact bytes and no CLI or compiler decoding failure occurs

#### Scenario: Preserve the existing program name

- **WHEN** the platform boundary admits one program-name byte sequence for `silk test`
- **THEN** HostInput index zero contains that same sequence for both standard and custom runners without synthesis from a runner path or identity

### Requirement: Test output reaches command standard output exactly once

The command SHALL provide evaluator standard output with a scoped byte-preserving sink to its own
standard output. Every runner write SHALL reach that sink once in original order. The command SHALL
NOT replay an already emitted evaluator transcript. A sink write failure SHALL flow through the
ordinary stream provider so StandardReporter can return ReportError and the standard runner can
return status 2. The sink SHALL close on every workflow exit without replacing the primary result.

#### Scenario: Forward one completed transcript

- **WHEN** the standard runner writes PASS, FAIL, logical-frame, and aggregate bytes successfully
- **THEN** command standard output receives those exact bytes once and in write order

#### Scenario: Fail one output write

- **WHEN** the command standard-output sink rejects a standard-runner write
- **THEN** the failure becomes ReportError, the runner returns 2, later cases do not run, and the sink and runner resources close once

### Requirement: Test command statuses preserve runner and tooling classes

After successful test compilation with the standard runner, the command SHALL return status 0 for
all selected cases passing, 1 for any selected test failure, and 2 for no explicit filter match or
runner input, allocation, output, or reporting infrastructure failure. A custom runner SHALL retain
the canonical entry contract: unit or successful closed Effect completion maps to 0, unhandled typed
entry failure maps to 1, and ordinary `main() -> i32` preserves any returned status exactly.
Configuration, storage, and
other pre-execution operational failures SHALL retain the existing command status 2. Source or
eligibility rejection SHALL retain the existing pre-execution source-rejection status 1. Fatal
evaluator traps and all other non-entry-completion evaluator classifications SHALL retain their
existing command reporting and process behavior instead of being converted to a runner status.

#### Scenario: Preserve selected test failure

- **WHEN** a valid compiled suite completes with one or more failed selected cases and no infrastructure failure
- **THEN** `silk test` exits 1

#### Scenario: Preserve a custom status

- **WHEN** a valid custom ordinary `main() -> i32` returns 42
- **THEN** `silk test` exits 42 without interpreting it as a standard-runner status

#### Scenario: Preserve no-match infrastructure status

- **WHEN** valid explicit filters match no tests and the standard runner returns 2
- **THEN** `silk test` exits 2 rather than treating the empty selection as success

#### Scenario: Diagnose before execution

- **WHEN** test source has an eligibility diagnostic
- **THEN** `silk test` exits with the source-rejection class and no runner status is fabricated

#### Scenario: Prefer an operational pre-execution failure

- **WHEN** one test source is rejected and another configured source has an operational read failure
- **THEN** the command reports both under existing workflow rules, exits 2, and invokes no runner

#### Scenario: Preserve another evaluator termination

- **WHEN** evaluation ends in an existing non-entry-completion classification other than a trap
- **THEN** `silk test` uses that classification's existing command reporting and process behavior without fabricating status 0, 1, or 2
