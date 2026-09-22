## REMOVED Requirements

### Requirement: Fingerprints do not skip execution in the first runner

**Reason**: Dependency-complete execution identities and strict pass records now authorize exact
per-test reuse; the local authored fingerprint remains insufficient by itself.

**Migration**: Invoke `silk test --no-cache` when every selected test must execute, including tests
that depend on undeclared runtime inputs or earlier tests' side effects.

## ADDED Requirements

### Requirement: Dependency-complete hits may skip exact tests

The bundled runner SHALL treat authored fingerprints as local content metadata only. For each
selected test it SHALL accept a skip only from the validated invocation plan for that exact
declaration and its dependency-complete execution identity. A skipped test SHALL be reported as a
cached pass without invoking its body. A miss, ineligible identity, invalid plan, or disabled cache
SHALL NOT skip execution. Selected misses SHALL retain sequential invocation, failure recovery,
cleanup, trap, filtering, and exit semantics.

#### Scenario: Skip an admitted passing test

- **WHEN** the invocation plan contains one exact valid hit for a selected test
- **THEN** the runner reports that test as cached and does not call its function

#### Scenario: Ignore a local fingerprint match

- **WHEN** the authored fingerprint matches an earlier run but no valid dependency-complete hit is admitted
- **THEN** the runner executes the selected test

#### Scenario: Keep executed ordering

- **WHEN** cached tests occur between two selected misses in catalog order
- **THEN** the two misses execute once each in their original relative order with cleanup completed between them
