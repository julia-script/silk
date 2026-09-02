## ADDED Requirements

### Requirement: Test is a canonical ordinary-source actor

Canonical `silk.test` SHALL expose safe opaque `Function` and `StackPath` values, the borrowed
ordered inventory, stable function IDs, Passed and Failed outcomes, outcome inspection, and one
safe per-handle run operation over the sealed primitives. Its reusable policy and types SHALL be
visible ordinary Silk source. Renaming an equivalent source wrapper MUST NOT change its eligibility
or behavior, and no public operation SHALL expose an erased failure value or general callable.
`Test.pathLength(&StackPath)` SHALL return the exact frame count and
`Test.pathFrame(&StackPath, index)` SHALL return `StackFrame | None`. A present immutable
StackFrame view SHALL expose public `function`, `source`, `start`, and `end` fields containing the
complete canonical function-identity bytes, canonical source-module bytes, and half-open source
byte offsets. Lookup beyond the path length SHALL return None without moving, mutating, or
allocating from the path.
`Test.isFailed(&Outcome)` SHALL classify Passed as false and Failed as true without moving,
mutating, allocating from, reporting, or otherwise consuming the Outcome.

#### Scenario: Iterate and invoke through Test

- **WHEN** an ordinary custom runner borrows `Test.functions()`, reads an ID, and runs one copied Function handle
- **THEN** it receives the compiler-defined canonical order and one closed Outcome without compiler recognition of the Test actor name

#### Scenario: Inspect a failed path

- **WHEN** custom runner source receives a Failed outcome
- **THEN** it can inspect every immutable logical frame's public canonical function, source, start, and end fields while remaining unable to construct or mutate StackPath

#### Scenario: Check a path index

- **WHEN** a custom runner asks for a frame at the path length or any larger index
- **THEN** `Test.pathFrame` returns None without allocating, trapping, or changing ownership of the path

#### Scenario: Classify an outcome without consuming it

- **WHEN** ordinary runner source calls `Test.isFailed` on borrowed Passed and Failed outcomes
- **THEN** it receives false and true respectively without moving either outcome, allocating, or reporting

#### Scenario: Keep compiler wrappers opaque

- **WHEN** ordinary source attempts to construct or inspect Function, construct or mutate StackPath, call or convert Function as a general callable, or access an erased failure payload
- **THEN** semantic analysis rejects each operation because only the safe Test inspection and invocation surface is public

### Requirement: Initial assertions are silent scalar checks

`Test.assert(condition)` SHALL return unit when `condition` is true and fail with
`Test.AssertionError`
when false. It SHALL emit no output, event, value, source location, or report. `Test.equalBytes`
SHALL accept borrowed `u8` slices, assert equal lengths, then assert elements from index zero upward,
stopping at the first mismatch. It SHALL retain no mismatch index, actual byte, or expected byte and
SHALL be implemented in ordinary source over slice access and `Test.assert`.

#### Scenario: Assert true silently

- **WHEN** `Test.assert(true)` runs inside a test
- **THEN** it returns unit without reporting or adding a requirement

#### Scenario: Recover a false assertion

- **WHEN** test source recovers `Test.AssertionError` from `Test.assert(false)` and returns normally
- **THEN** the test passes and no assertion-side report remains

#### Scenario: Compare equal byte slices

- **WHEN** two borrowed byte slices have the same length and element values
- **THEN** `Test.equalBytes` returns unit without allocation or reporting

#### Scenario: Stop at the first byte mismatch

- **WHEN** byte slices differ in length or at an element
- **THEN** `Test.equalBytes` returns `Test.AssertionError` at the first failed assertion and preserves no compared values

### Requirement: Reporter consumes public completed-case events

`PassedCase.function`, `FailedCase.function`, `FailedCase.path`, and `Event.value` SHALL be public.
`Test.event(function, outcome)` SHALL consume one completed Outcome into the corresponding Event.
`Test.Reporter` SHALL be an ordinary mutable runner service whose report operation consumes one
Event and may fail only with `Test.ReportError`. `Test.ReportError` SHALL be a public empty,
detached struct that any ordinary Reporter implementation can construct; it SHALL have no hidden
compiler identity or payload. `Test.report` SHALL delegate to the lexically
provided Reporter after test execution. Reporter SHALL NOT be a test requirement, and report
failure MUST NOT alter the already completed test outcome.

#### Scenario: Count a failed case in a custom reporter

- **WHEN** a custom runner converts Failed to an Event and provides a mutable counting Reporter around `Test.report`
- **THEN** the public event fields identify the function and complete path and the reporter increments its state once

#### Scenario: Omit reporting deliberately

- **WHEN** a custom runner consumes outcomes without calling `Test.report`
- **THEN** test invocation and outcome semantics remain unchanged and no Reporter provider is required

#### Scenario: Classify report failure as infrastructure

- **WHEN** reporting a completed failed case exits with `Test.ReportError`
- **THEN** the case remains failed and the runner separately classifies the reporting operation as infrastructure failure

### Requirement: Standard filtering is raw ASCII-folded substring matching

The standard runner SHALL compare each raw filter byte sequence against the UTF-8 fully qualified
test ID using byte-substring matching after folding only ASCII bytes `A` through `Z` to lowercase.
Every other byte SHALL remain exact; filters need not decode as UTF-8. No filters SHALL select all
entries. Multiple filters SHALL use ordered OR selection while preserving canonical inventory order.
Empty filters, glob syntax, regular expressions, exclusions, and tags SHALL receive no special
meaning. Consequently an empty filter SHALL behave as the ordinary empty substring and select
every inventory entry. No-match SHALL occur only when the complete nonempty OR filter set selects
zero entries.

#### Scenario: Match ASCII case-insensitively

- **WHEN** a test ID contains `fillBytes` and a raw filter is `FILLBYTES`
- **THEN** the entry is selected through ASCII folding

#### Scenario: Keep non-ASCII and invalid bytes exact

- **WHEN** a raw filter contains non-ASCII or invalid UTF-8 bytes
- **THEN** those bytes compare exactly and no decoding, normalization, or Unicode case folding occurs

#### Scenario: Preserve order under several filters

- **WHEN** several filters select overlapping entries
- **THEN** each selected entry appears once in original canonical inventory order

#### Scenario: Treat pattern-looking bytes literally

- **WHEN** filters contain empty bytes or bytes spelling `*`, `.`, `[`, `!`, or `tag:`
- **THEN** the empty filter selects every ID and every nonempty filter is compared as that exact literal byte substring without pattern or tag semantics

#### Scenario: Let one OR filter miss while another matches

- **WHEN** one explicit filter matches no ID and another explicit filter matches at least one ID
- **THEN** the matching entries are selected once in canonical order and the suite is not classified as no-match

### Requirement: The standard runner reports deterministically

Canonical `silk.test_runner` SHALL be the separately designated default runner root. It SHALL
expose an ordinary `pub fn main() -> i32` that delegates exactly once to the closed OS-host edge.
Selecting that module as the runner root SHALL NOT add its declarations to the test inventory
unless it is also independently designated as a test root. The standard-library manifest and
generated shipped-source table SHALL include both `silk.test` and `silk.test_runner`.
The standard runner SHALL execute each selected entry exactly once in canonical order in one
process. It SHALL create fresh standard Reporter state for each case, convert the completed outcome
to one event, report it, and aggregate pass and failure counts. It SHALL return status 0 when every
selected case passes, 1 when any selected case fails, and 2 when explicit filters match no entries
or input, allocation, output, or reporting infrastructure fails. A trap SHALL remain fatal.
Infrastructure status 2 SHALL override any previously accumulated selected-case status. A
mid-suite ReportError SHALL reclaim the current Event and owned path, stop before invoking any later
case, and return 2. The runner's closed OS-host edge SHALL construct and lexically provide ordinary
OsHostInput, Allocator, and standard-output providers, ignore host argument zero, and copy argument
indices one onward to owned filter Bytes in exact order. It SHALL release every acquired filter and
format/output resource on success, typed failure, and early infrastructure exit.

#### Scenario: Run all tests without filters

- **WHEN** the inventory has passing and failing entries and no filters are supplied
- **THEN** each entry runs and reports once in canonical order and the final status is 1

#### Scenario: Run an empty inventory without filters

- **WHEN** the inventory is empty and no filters are supplied
- **THEN** the standard reporter writes exactly `0 passed; 0 failed\n` and the runner returns 0

#### Scenario: Load the canonical runner root

- **WHEN** the standard runner is selected for a suite
- **THEN** `silk.test_runner::main` is loaded as the distinct runner root and delegates to the closed edge exactly once without contributing declarations to that suite's inventory

#### Scenario: Report no matches

- **WHEN** the complete nonempty explicit OR filter set selects no inventory entry
- **THEN** the standard reporter emits `0 matched` and the runner returns 2 without invoking a test

#### Scenario: Keep reporter state fresh per case

- **WHEN** two selected cases run under the standard runner
- **THEN** each report receives fresh per-case standard Reporter state while suite aggregation remains deterministic

#### Scenario: Acquire raw filters at the closed edge

- **WHEN** host input contains runner argument zero followed by several filter byte sequences
- **THEN** the edge skips index zero, copies indices one onward unchanged and in order under lexical OsHostInput and Allocator provision, and releases every owned filter after the suite

#### Scenario: Roll back partial filter acquisition

- **WHEN** HostInput or allocation fails after one or more filter values were acquired
- **THEN** every acquired filter is reclaimed exactly once, no test is invoked, and the closed edge returns 2

#### Scenario: Stop on a mid-suite report failure

- **WHEN** an earlier selected case has completed and reporting a later completed Event returns ReportError
- **THEN** the Event and any owned StackPath are reclaimed exactly once, no later case runs, and status 2 overrides accumulated pass or failure status

### Requirement: Standard presentation filters a complete path without changing it

The standard reporter SHALL write exact UTF-8 case lines `PASS <id>\n` and `FAIL <id>\n`, write each
retained failed logical frame as `  at <canonical-function-identity>\n`, and after a completed
nonempty selection write `<passed> passed; <failed> failed\n`. A complete explicit-filter miss
SHALL instead write exactly `0 matched\n`. It MUST NOT claim an assertion-specific reason because
the failure value is erased. Presentation SHALL retain test and called-helper frames while omitting
only the exact canonical logical identities `silk.test_runner::main`,
`silk.test_runner::runFromOsHost`, `silk.test_runner::runSuite`, and `silk.test::run`, in addition
to physical entry adapters that the prerequisite logical path already excludes. It SHALL traverse
and print retained logical frames in their existing outer-to-inner order. No prefix, suffix,
unqualified-name, or spelling-similarity rule SHALL omit a frame; every other logical frame,
including a user helper named `runSuite`, SHALL remain. The initial
source reporter SHALL print canonical logical identities rather than invent a filesystem-path
resolver; any tooling display of physical paths SHALL reuse existing project source metadata and
the existing FileSourceResolver convention. Filtering MUST operate on presentation only: the
Event's owned StackPath SHALL remain complete and a custom reporter SHALL inspect every logical
frame.

StandardReporter SHALL capture ordinary borrowed output and allocation dependencies when it is
constructed and translate any formatting, allocation, or output failure to its sole
`Test.ReportError` failure channel. `Test.report` SHALL retain no residual service requirement
other than `? &mut Test.Reporter`; StandardReporter construction, use, and cleanup MUST NOT expose
HostInput, Allocator, output, or display requirements through the Reporter contract.

#### Scenario: Present an assertion helper failure

- **WHEN** a test fails inside `silk.test::equalBytes`
- **THEN** standard output writes the exact FAIL line and canonical test/helper frame lines while omitting runner adapters and retaining no assertion-specific reason

#### Scenario: Omit only exact runner identities

- **WHEN** a failed path contains the four canonical infrastructure identities, ordinary test and helper frames, and a user helper named `runSuite`
- **THEN** standard presentation omits exactly the four canonical identities and retains every other frame in original outer-to-inner order

#### Scenario: Inspect frames omitted from standard output

- **WHEN** a custom reporter receives the same failed Event
- **THEN** it can inspect the complete immutable StackPath including any logical infrastructure frames omitted only by standard presentation

#### Scenario: Render passing, failing, and aggregate lines

- **WHEN** one passing and one failing case complete without infrastructure failure
- **THEN** output contains their exact PASS and FAIL lines in canonical case order followed by exactly `1 passed; 1 failed\n`

#### Scenario: Close reporting failures into ReportError

- **WHEN** standard formatting allocation or output fails while consuming an Event
- **THEN** StandardReporter returns only Test.ReportError, releases all temporary formatting state and the consumed Event/path exactly once, and introduces no residual service requirement

### Requirement: Generated documentation states the initial testing boundary

Generated standard-library documentation SHALL describe wrapper opacity, checked frame access,
borrowed Outcome classification and consuming Event conversion, AssertionError recovery,
equalBytes limits, Event ownership, Reporter and constructible ReportError behavior, raw literal
filtering, the canonical runner root, exit statuses, generic failure presentation, and fatal-trap
behavior. It SHALL NOT promise failure values or messages, mismatch data, filesystem paths, skips,
generic equality, or compiled-engine modes.

#### Scenario: Read the generated Test documentation

- **WHEN** a user reads the generated documentation for the initial testing API
- **THEN** every supported behavior and limitation is stated without implying any excluded capability
