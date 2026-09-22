# silk-test-result-caching Specification

## Purpose

Define safe persistent reuse of individual passing Silk test results from dependency-complete
execution identities while preserving ordinary runner output and failure semantics.

## ADDED Requirements

### Requirement: Execution identity is complete and per test

Each discovered test SHALL have a versioned execution identity distinct from its local authored
fingerprint. An eligible identity SHALL include the canonical declaration identity and authored
header/body, every transitively reachable runtime implementation and hidden body, demanded
compile-time helper/default/predicate dependency, selected provider and implementation, published
compilation profile and its bootstrap dependencies, runner/runtime policy, compiler distribution,
and resolved external native execution inputs. It SHALL exclude source presentation, filters,
physical output destinations, generated whole-test-artifact bytes, unrelated declarations, and
other tests that are not dependencies. If completeness cannot be established, only that test SHALL
be ineligible for reuse; the system MUST NOT substitute a whole-file or whole-suite key.

#### Scenario: Change one private helper in a shared file

- **WHEN** test A and test B share a source file and only a private helper transitively used by A changes implementation
- **THEN** A receives a different execution identity and B retains its previous identity

#### Scenario: Change a shared transitive helper

- **WHEN** a runtime or compile-time helper is transitively demanded by two tests and its canonical authored body changes without changing its signature
- **THEN** both tests receive different execution identities

#### Scenario: Preserve presentation-only reuse

- **WHEN** comments, whitespace, source coordinates, or an unrelated declaration change while one test's complete execution dependency graph is unchanged
- **THEN** that test retains its execution identity

#### Scenario: Refuse incomplete dependency authority

- **WHEN** the compiler cannot prove a complete execution dependency closure for one test
- **THEN** that test executes and publishes no reusable pass record while independently eligible tests remain reusable

### Requirement: Persistent records authorize completed passes only

One valid record SHALL authorize only the exact execution identity named by its storage key and
payload. The record format SHALL be versioned, strictly decoded, bounded, integrity-checked, and
limited to a completed `Passed` outcome. Missing, stale, incompatible, incomplete, corrupt,
oversize, duplicate, or mismatched data SHALL authorize no pass and SHALL cause execution. Failed,
interrupted, trapped, or operationally incomplete tests SHALL have no reusable result record.

#### Scenario: Reuse an unchanged pass across processes

- **WHEN** an eligible selected test completed as passed, its record was published, and a later process computes the same execution identity
- **THEN** the later invocation may report the test as cached without invoking its body

#### Scenario: Rerun a previous failure

- **WHEN** a selected test failed on the previous invocation and all declared execution inputs remain unchanged
- **THEN** the next invocation executes the test because no failure record can authorize reuse

#### Scenario: Reject a damaged record

- **WHEN** a record is truncated, has an unknown field or version, names another identity, fails integrity validation, or exceeds its bound
- **THEN** the test executes and the damaged record does not become a passed result

### Requirement: Runner exchange is isolated and authoritative

Cache admission and executed outcomes SHALL use a bounded versioned exchange channel separate from
the child process's inherited stdout and stderr. The complete input plan SHALL identify every
discovered declaration, carry its opaque compiler-owned execution identity when eligible, and
state whether an exact valid hit is available. The source runner SHALL validate declaration order,
treat the identity as opaque, and echo it for selected dispositions rather than constructing cache
authority. The complete result receipt SHALL bind a fresh invocation nonce, the input plan,
selected dispositions, counts, and completed exit status. User-written stdout or stderr bytes MUST
NOT be parsed as cache authority. A normally exiting process with an invalid or incomplete receipt
SHALL publish no records and SHALL be treated as runner operational failure; abnormal process
termination SHALL retain its existing meaning.

#### Scenario: Test output resembles protocol data

- **WHEN** a test writes bytes to stdout or stderr that resemble result records
- **THEN** those bytes remain user output and cannot create, suppress, or alter a cache record

#### Scenario: Reject an incomplete receipt

- **WHEN** the runner exits normally but its result receipt is missing its completion frame or disagrees with the input plan, nonce, counts, or exit status
- **THEN** the invocation publishes no new pass records and exits with runner operational status 2

#### Scenario: Preserve a trap

- **WHEN** a test traps before the runner completes its result receipt
- **THEN** the process retains abnormal termination and no partial receipt authorizes publication

### Requirement: Cache failures degrade to execution

Expected result-cache read failures and record-decoding failures SHALL make the affected test a
cache miss and permit execution. Expected publication failures SHALL preserve the authoritative
runner outcome while leaving that pass unavailable for later reuse. Cache handling SHALL recover
only typed expected cache failures; interruption and unexpected defects SHALL retain their Effect
semantics. Cache failures SHALL remain observable separately from test failures.

#### Scenario: Storage read fails

- **WHEN** the result store cannot read one eligible test record through an expected typed failure
- **THEN** that test executes and the cache failure is not reported as a failed test

#### Scenario: Storage publication fails

- **WHEN** an executed test passes but publishing its pass record fails through an expected typed failure
- **THEN** the command preserves the test's pass and completed exit status while reporting the cache publication failure separately

### Requirement: Reuse preserves selection and sequential semantics

The ordinary source runner SHALL remain the authority for exact file/name filtering and catalog
order. A selected valid hit SHALL be reported as cached and SHALL not invoke the test body. All
selected misses SHALL execute sequentially in their existing relative catalog order and finish
ordinary cleanup before the next executed test. The summary SHALL report discovered, selected,
cached, executed, passed, and failed counts, with cached passes included in passed. Zero selection
SHALL remain successful. The identity contract SHALL NOT claim to track arbitrary runtime
filesystem, environment, network, time, or other-test side effects; users of those inputs SHALL
bypass result reuse.

#### Scenario: Mix cached and executed tests

- **WHEN** three selected tests contain one valid pass hit, one miss that passes, and one miss that fails
- **THEN** the hit is not invoked, the two misses execute sequentially, the summary reports cached 1, executed 2, passed 2, failed 1, and the runner exits 1

#### Scenario: Keep filters out of identity

- **WHEN** two invocations use different file or name filters without changing compilation or test dependencies
- **THEN** execution identities remain equal and each invocation applies its own runtime selection before counting cached or executed tests

#### Scenario: Bypass undeclared external inputs

- **WHEN** a test depends on changing runtime state that the execution identity does not declare
- **THEN** the user can require execution through the result-cache bypass rather than receiving a claim that the state was tracked
