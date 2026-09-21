## Purpose

Provide a minimal runner and assertions in ordinary Silk over the public test-discovery primitives, retaining normal execution and failure semantics.

## ADDED Requirements

### Requirement: Test policy is ordinary source

The bundled runner SHALL implement selection, invocation, recovery, reporting, and exit policy in Silk using the same public discovery operations available to another source runner. Compiler phases SHALL NOT recognize assertion, runner, or result actors by spelling or expose test-execution/filtering/reporting intrinsics. A minimal source assertion helper SHALL accept a boolean condition and message, succeed with unit when true, and produce an ordinary typed assertion failure when false without requiring an ambient service.

#### Scenario: Fail an assertion

- **WHEN** an Effect test runs the assertion helper with false and a message
- **THEN** it produces an ordinary typed failure recoverable through existing Effect operations

#### Scenario: Rename source policy

- **WHEN** an equivalent runner or assertion wrapper is supplied under a different legal source name
- **THEN** its behavior follows ordinary source calls and the same intrinsics without compiler name privilege

### Requirement: Sequential execution preserves normal outcomes and cleanup

The runner SHALL invoke matching tests once each in catalog order, waiting for the current invocation and its ordinary structured cleanup to finish before starting the next. Ordinary unit return and successful unit Effect completion SHALL count as pass. Any typed failure SHALL count as one failed test and permit continuation, without imposing a payload formatting interface. Failure payloads SHALL be cleaned exactly once and available source diagnostic context SHALL be reported through existing observation facilities. The runner SHALL identify the active test before invoking it. A trap SHALL retain fatal process semantics, without recovery or guaranteed cleanup.

#### Scenario: Continue after typed failure

- **WHEN** a selected test fails with an ordinary typed value and the next selected test succeeds
- **THEN** the first is reported failed, its structured cleanup finishes, and the second runs and is reported passed

#### Scenario: Report a trap boundary

- **WHEN** a test traps after its identity has been printed
- **THEN** the process terminates abnormally and the runner does not claim that remaining tests ran

### Requirement: The first runner supplies no test services or scheduler

Each test invocation accepted by the bundled runner SHALL have no unsatisfied service requirement and SHALL satisfy the existing non-parking host boundary for its complete execution. Tests SHALL provide their own services, mocks, and any scheduler or execution owner needed internally. Runner-owned process input, diagnostics, and output SHALL NOT imply ambient provision to test bodies. An unsatisfied requirement or parking boundary SHALL produce an ordinary compile-time rejection regardless of runtime filtering. Discovery callable types SHALL remain precise so another source host can explicitly supply providers.

#### Scenario: Provide a service locally

- **WHEN** an Effect test closes its service requirements around a helper or nested Effect
- **THEN** the runner accepts the resulting unit-success computation and normal ownership governs provider cleanup

#### Scenario: Reject an unprovided service

- **WHEN** a discovered test leaves a service requirement unsatisfied by its invocation in the builtin runner
- **THEN** the test executable is rejected even if a runtime name filter would not select that test

### Requirement: Runtime filters use literal name and exact file predicates

The runner SHALL select by an optional exact normalized project-relative source path and an optional case-insensitive literal substring of the declared test name. Name matching SHALL fold ASCII letters independently of locale, preserving other bytes literally; names use the language's ASCII identifier repertoire. An empty name pattern SHALL match every name. Names SHALL exclude module/path decoration. When both predicates are present they SHALL combine with AND. Globs, regular expressions, path substrings, and Unicode normalization SHALL NOT be interpreted. Filters SHALL NOT alter discovery, source loading, or compilation.

#### Scenario: Match mixed case literally

- **WHEN** a test named `parsesInvalidInput` is considered with pattern `INVALID`
- **THEN** the name predicate matches

#### Scenario: Combine file and name

- **WHEN** matching test names occur in two files and both a file filter and name filter are supplied
- **THEN** only matching names in the exact selected file execute

#### Scenario: Preserve literal metacharacters

- **WHEN** a pattern contains `*` or `.`
- **THEN** those characters are searched literally and receive no glob or regular-expression meaning

### Requirement: Results and empty selections are explicit

The runner SHALL print each executed test's pass/fail status and a final summary containing discovered, selected, passed, and failed counts. It SHALL return status 0 if all selected tests pass, including zero selected tests with an explicit zero-selected summary, and status 1 if any test has a typed failure. Runner operational failures SHALL use status 2. Fatal process termination SHALL remain distinguishable from ordinary completion. Exact decorative formatting, color, and timing output are not required.

#### Scenario: Select no tests

- **WHEN** the graph has no tests or filters match none
- **THEN** the runner reports zero selected tests and exits 0 without invoking a test

#### Scenario: Report a mixed result

- **WHEN** two selected tests pass and one fails with a typed failure
- **THEN** the summary reports selected 3, passed 2, failed 1 and the runner exits 1

### Requirement: Fingerprints do not skip execution in the first runner

The builtin runner SHALL neither read nor write a persistent test-result cache and SHALL execute every selected test on each invocation, independent of previous fingerprints or results. Fingerprint metadata SHALL remain available to source consumers as authored-content information only.

#### Scenario: Rerun unchanged source

- **WHEN** the same compiled test executable is invoked twice with the same filters
- **THEN** every matching test executes on both invocations even when all authored fingerprints are unchanged
