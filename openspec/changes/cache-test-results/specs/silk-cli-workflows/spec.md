## ADDED Requirements

### Requirement: Project test controls result reuse independently

`silk test` SHALL enable persistent per-test pass reuse by default and SHALL accept `--no-cache` to
bypass every test-result-cache read and write for that invocation without deleting or mutating
existing result records. The flag SHALL NOT disable or alter compiler semantic, backend, object, or
final-artifact caches. Existing root selection,
runtime filters, inherited user output, compilation diagnostics, abnormal termination, and exit
classifications SHALL remain in force. Completed output SHALL distinguish cached and executed test
counts.

The workflow SHALL use compact uncached exchange when either per-test exchange file cannot fit its
declared bound, including long identities and large discovered counts even without hits. It SHALL
choose this mode before result-cache lookup, perform no result-cache reads or writes, and publish
no records from its aggregate receipt. `--no-cache` SHALL select the same compact mode directly.
Neither case SHALL limit discovery, bypass compilation of unselected tests, move filtering into
the CLI, truncate output, or change completed/abnormal exit behavior.

#### Scenario: Reuse by default

- **WHEN** a second process invokes `silk test` with one selected eligible unchanged passing test and the per-test exchange fits its bounds
- **THEN** the command reports one cached test and zero executed tests without invoking its body

#### Scenario: Bypass only result caching

- **WHEN** a user invokes `silk test --no-cache`
- **THEN** every selected test executes, no result record is read or written, and compilation caches remain governed by their existing controls

#### Scenario: Preserve behavior when exchange metadata is too large

- **WHEN** long declaration identities or a large test catalog exceed either per-test file bound, regardless of available hits or runtime selection size
- **THEN** all discovered tests still compile, selected tests execute through the bounded compact exchange with inherited output and cached zero, ordinary statuses remain authoritative, and no result-cache reads or writes occur

#### Scenario: Preserve pre-execution failures

- **WHEN** compilation, configuration, storage required for compilation, or toolchain preparation fails before the runner starts
- **THEN** the command retains its existing status and diagnostics and fabricates no cached or executed test result
