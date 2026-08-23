## ADDED Requirements

### Requirement: Evaluation provides the individual test invocation boundary

Evaluation SHALL invoke one closed marked test from its opaque inventory handle through a uniform
target-neutral adapter. It SHALL translate normal return and unhandled typed failure to the closed
test outcomes while retaining the evaluator's existing logical frame ordering and source spans.
The adapter SHALL perform ordinary cleanup before returning the outcome, MUST NOT expose or retain
the erased failure value, and SHALL leave traps on the existing fatal termination path.

#### Scenario: Capture nested logical frames

- **WHEN** a marked test calls an ordinary assertion helper that exits with an unhandled typed failure
- **THEN** Failed owns the complete test-and-helper logical frames with their canonical identities and source spans in the evaluator's existing order

#### Scenario: Clean affine state before returning Failed

- **WHEN** nested test frames own affine values and the unhandled typed failure payload owns a separate affine cleanup witness
- **THEN** every exited frame owner and the erased failure payload are cleaned exactly once before Failed is returned and the failure value is not retained

#### Scenario: Preserve the owned path lifecycle

- **WHEN** repeated Failed outcomes are dropped or their paths move through completed-case consumption including downstream failure
- **THEN** each immutable path snapshot transfers without duplication and is reclaimed exactly once

#### Scenario: Invoke heterogeneous failure rows uniformly

- **WHEN** two eligible tests have distinct typed failure rows
- **THEN** the same opaque invocation operation runs either handle and returns the same closed Outcome shape without a general erased callable value

#### Scenario: Clean affine state before returning Passed

- **WHEN** a marked test owns affine frame values, recovers an ordinary typed failure, and then returns normally
- **THEN** every frame and recovered failure value is cleaned exactly once before Passed is returned

#### Scenario: Preserve non-outcome evaluator termination

- **WHEN** one test invocation reaches an existing evaluator termination other than normal return or unhandled typed failure
- **THEN** that termination retains its existing classification outside Outcome and stops the runner execution without fabricating Passed or Failed
