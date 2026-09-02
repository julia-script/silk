## Purpose

Defines the source marker, eligibility rules, deterministic inventory, opaque handles, and closed
per-test invocation outcomes that let ordinary Silk source implement test runners.

## ADDED Requirements

### Requirement: Test declarations are private closed Effects

A `test` declaration SHALL be a private named top-level non-generic Effect function with zero
parameters, unit success, any typed failure row, and an empty residual service-requirement row. The
marker SHALL NOT add a failure, requirement, visibility, or call semantic of its own. A public,
ordinary, generic, parameterized, non-unit, or residually requiring marked declaration SHALL be
unavailable for inventory membership. Application services SHALL be provided inside the test body;
Reporter SHALL be runner-facing and SHALL NOT participate in eligibility.

#### Scenario: Admit a closed test with an application failure

- **WHEN** a private top-level zero-parameter non-generic Effect function returns unit, may fail with an application error, and provides all application services inside its body
- **THEN** the declaration is eligible exactly once without gaining an implicit Test failure or Reporter requirement

#### Scenario: Reject every open declaration shape

- **WHEN** marked declarations are public, ordinary rather than effectful, generic, parameterized, non-unit, or retain a residual service requirement
- **THEN** each invalid declaration is diagnosed and none appears as a partial inventory entry

#### Scenario: Preserve ordinary marked-function semantics

- **WHEN** otherwise equivalent marked and unmarked private Effect functions are analyzed and called ordinarily
- **THEN** their body resolution, visibility, callable contract, declared success/failure/requirement rows, HIR, MIR, and ordinary call behavior are identical apart from the canonical marker and test-eligibility facts

### Requirement: Test inventory is root-scoped and canonically ordered

A test compilation SHALL derive inventory membership from the de-duplicated union of the explicit
test roots' transitive module closures. It MUST NOT scan an unrelated source file. Eligible entries
SHALL be ordered first by exact canonical module identity and then by declaration source order
within that module, independently of test-root order. The separately designated executable runner
root SHALL be composed into the same compilation and MAY also appear in the test-root set, but
declarations reachable only through its runner role MUST NOT enter the inventory; a module shared
by test and runner roles SHALL retain one canonical identity and SHALL enter inventory only through
its test-root role. A runnable inventory SHALL be withheld unless recoverable analysis and ownership
checking complete without any source diagnostic that makes a test-root or runner-root declaration
unavailable; such damage remains a pre-execution test-compilation error rather than an executable
trap or case outcome.

#### Scenario: Deduplicate two roots with one shared import

- **WHEN** two test roots reach the same marked declaration through a shared imported module
- **THEN** the declaration appears once at its canonical module-and-source-order position

#### Scenario: Exclude an unrooted source file

- **WHEN** a package contains a marked declaration in a file outside every test-root closure
- **THEN** that declaration is not loaded or added merely because the file exists below the source root

#### Scenario: Keep test artifacts out of an ordinary build

- **WHEN** an ordinary non-test compilation reaches source containing marked declarations
- **THEN** it publishes no test inventory and generates or roots no per-test adapter, test runtime, or test-only code-size cost

#### Scenario: Keep a runner-only test out of inventory

- **WHEN** a marked declaration is reachable only from the selected runner root
- **THEN** it is excluded, while the same module also reached through a test root is de-duplicated and contributes its eligible declarations

#### Scenario: Withhold a runnable inventory for an invalid body

- **WHEN** an otherwise eligible marked header or selected runner has a body semantic or ownership diagnostic that ordinary evaluation would lower to a generated trap
- **THEN** analysis retains the diagnostic but publishes no runnable test inventory or executable runner and no test case is invoked

### Requirement: Inventory handles expose identity without general callability

Each inventory entry SHALL be an immutable opaque Copy handle supplied by the current test
compilation. Ordinary source SHALL be able to borrow the complete ordered handle slice, inspect its
length, index it, copy a handle token, and obtain the stable fully qualified ID
`canonical/module::declarationName` as an immutable borrowed UTF-8 byte view into compilation-owned
metadata without allocation or copying. Ordinary source MUST NOT construct a handle, call it
directly, convert it to a general callable value, or inspect a representation.

#### Scenario: Iterate the borrowed inventory

- **WHEN** a runner obtains the inventory slice and visits it with length, indexing, and a while loop
- **THEN** every handle can be copied and passed to test operations in canonical order without allocation or ownership recovery

#### Scenario: Read one exact canonical test ID

- **WHEN** a runner reads metadata for declaration `fillBytes` in canonical module `std/random`
- **THEN** the borrowed immutable UTF-8 ID bytes spell exactly `std/random::fillBytes` without allocation or copying, while another declaration in that module retains its source-order inventory position

#### Scenario: Refuse forged or callable handles

- **WHEN** ordinary source attempts to construct an inventory handle or use one as an erased function pointer
- **THEN** the operation is unavailable and no general callable conversion is published

### Requirement: Per-test invocation returns one closed outcome

Invoking an inventory handle SHALL run exactly that marked Effect with its ordinary semantics.
Normal return SHALL produce Passed. Any unhandled typed failure SHALL produce Failed without
exposing the erased failure value. Failed SHALL own one immutable `StackPath` containing the
complete ordered logical frames active at the failure boundary, each with canonical function
identity and source span. Runner operations SHALL be able to inspect but not construct or mutate
the path. Frame lookup beyond the path length SHALL return checked absence without trapping,
failing, allocating, moving, or mutating the path. The invocation SHALL have no Reporter
requirement. A runtime trap SHALL remain fatal and abort the suite rather than become Failed. Every
other existing evaluator termination outside normal return and unhandled typed failure SHALL retain
its existing classification outside Outcome and stop runner execution without fabricating a case
outcome.

#### Scenario: Return Passed after normal completion

- **WHEN** one eligible marked Effect returns normally
- **THEN** its invocation performs ordinary exactly-once frame and value cleanup, produces Passed exactly once, and performs no report

#### Scenario: Capture any unhandled typed failure

- **WHEN** one eligible marked Effect exits with an unhandled assertion or application failure
- **THEN** invocation produces Failed with no erased failure value and with the complete outer-to-inner logical failure path in the evaluator's existing order owned by the outcome

#### Scenario: Preserve recovery inside a test

- **WHEN** a test deliberately recovers an assertion failure and then returns normally
- **THEN** invocation produces Passed because no unhandled failure crossed its boundary

#### Scenario: Keep traps fatal

- **WHEN** execution traps while an inventory entry runs
- **THEN** the suite aborts through the existing fatal trap path and no Failed outcome is fabricated

#### Scenario: Check a path index

- **WHEN** a runner requests a frame at the path length or a larger index
- **THEN** inspection returns checked absence without allocating, trapping, or changing ownership of the path

#### Scenario: Preserve another evaluator termination

- **WHEN** an invocation reaches an existing evaluator limit or blocked termination rather than normal return, typed failure, or runtime trap
- **THEN** that termination remains outside Outcome with its existing classification and stops runner execution without producing Passed or Failed

### Requirement: Owned failure paths preserve cleanup

The evaluator SHALL capture and transfer an owned failure path at the individual invocation
boundary without changing logical-frame semantics, retaining live failure values, duplicating
affine state, hiding unbounded allocation, or skipping ordinary Effect cleanup. Representation work
MUST NOT proceed until a focused characterization proves this boundary. If the proof cannot be
realized target-neutrally under those constraints, implementation SHALL stop and SLP-0004 SHALL
return to Candidate rather than weakening the path contract.

#### Scenario: Prove the path boundary before representation

- **WHEN** a nested failing test owns affine frame values and exits with an affine failure payload that owns a distinct cleanup witness
- **THEN** the characterization observes the existing complete outer-to-inner logical frames, exactly-once frame and failure-payload cleanup, one owned path transfer, and no retained erased failure value before a representation is selected

#### Scenario: Release the owned path exactly once

- **WHEN** Failed is dropped, moved through an equivalent completed-case container, or consumed after a downstream reporting failure across repeated invocations
- **THEN** the path snapshot transfers without duplication and its owned representation is reclaimed exactly once on every terminal route

#### Scenario: Reject an unsound path realization

- **WHEN** a proposed realization requires changed frame semantics, hidden unbounded allocation, or incomplete cleanup
- **THEN** the implementation gate fails and no reduced, borrowed, or presentation-filtered StackPath contract is substituted
