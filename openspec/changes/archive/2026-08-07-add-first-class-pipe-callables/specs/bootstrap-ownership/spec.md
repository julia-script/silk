## ADDED Requirements

### Requirement: Callable environments obey ordinary affine ownership

Ownership SHALL derive callable invocation mode from how its environment is accessed: read-only
captures permit shared reusable calls, mutated or exclusively borrowed captures require exclusive
reusable calls, and an invocation that consumes any captured owner is take-once. Callable moves,
borrows, aggregate storage, returns, and drops SHALL use the same ownership and dependency rules as
other values. A provider or owner retained by a callable MUST remain immovable and live as required
until the callable releases that dependency.

#### Scenario: Reject a second taking call

- **WHEN** one invocation consumes an owned capture and the same callable is invoked again
- **THEN** ownership rejects the second invocation and identifies the consumed environment slot

#### Scenario: Release captures on callable drop

- **WHEN** a callable with owned and borrowed captures is dropped without invocation
- **THEN** owned captures clean exactly once and every capture loan ends at that drop

#### Scenario: Reject provider movement while retained

- **WHEN** a callable retains a borrow from a provider and code attempts to move or drop that provider
- **THEN** ownership rejects the provider operation while permitting valid shared capability use

### Requirement: Pipeline application preserves ownership order

Ownership SHALL analyze the pipeline left value before constructing or accessing the right callable,
then transfer or borrow the left value according to the callable's leading parameter. Failures in
callable construction or invocation MUST NOT duplicate the left value or any capture.

#### Scenario: Pipe an affine value once

- **WHEN** an affine value is piped into a callable whose leading parameter consumes it
- **THEN** the source binding becomes moved exactly once before the callable result is available
