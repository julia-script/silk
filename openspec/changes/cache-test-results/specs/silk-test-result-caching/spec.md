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

- **WHEN** an eligible selected test completed as passed, its record was published, and a later process computes the same execution identity and admits a per-test exchange within its bounds
- **THEN** the later invocation may report the test as cached without invoking its body

#### Scenario: Rerun a previous failure

- **WHEN** a selected test failed on the previous invocation and all declared execution inputs remain unchanged
- **THEN** the next invocation executes the test because no failure record can authorize reuse

#### Scenario: Reject a damaged record

- **WHEN** a record is truncated, has an unknown field or version, names another identity, fails integrity validation, or exceeds its bound
- **THEN** the test executes and the damaged record does not become a passed result

### Requirement: Runner exchange is isolated and authoritative

Cache admission and executed outcomes SHALL use a bounded versioned exchange channel separate from
the child process's inherited stdout and stderr. In `PerTest` mode the complete input plan SHALL
identify every discovered declaration, carry its opaque compiler-owned execution identity when eligible, and
state whether an exact valid hit is available. The source runner SHALL validate declaration order,
treat the identity as opaque, and echo it for selected dispositions rather than constructing cache
authority. Its complete result receipt SHALL bind a fresh invocation nonce, the input plan,
selected dispositions, counts, and completed exit status. In `Uncached` mode the exchange SHALL
instead use the fixed-size plan and aggregate receipt defined below, without per-test authority.
Every receipt SHALL be admitted only against its input plan's mode. User-written stdout or stderr
bytes MUST NOT be parsed as cache authority. A normally exiting process with an invalid or incomplete receipt
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

### Requirement: Oversized exchanges use bounded uncached execution

Before result-cache lookup or encoded-file allocation, the workflow SHALL establish that both the
actual `PerTest` plan and the maximum complete receipt fit
`min(4096 + 256 * discoveredCount, 16 MiB)` bytes each. The calculation SHALL include complete UTF-8
identities, every frame, eligible execution identities even without hits, and all discovered tests
selected with the longest possible disposition encoding. Size overflow or inability to prove
either bound SHALL select `Uncached`; clearing hits, truncating identities, omitting declarations,
or splitting files MUST NOT substitute for this mode.

`Uncached` SHALL use exactly a 256-byte plan and a 256-byte aggregate completion receipt, with no
per-test list in either direction. The plan SHALL contain only 8-byte magic, 4-byte version,
4-byte mode, 32-byte nonce, 32-byte catalog digest, 8-byte discovered count, and 168 zero padding
bytes, in that order. The receipt SHALL contain only 8-byte magic, 4-byte version, 4-byte mode,
32-byte echoed nonce, 32-byte SHA-256 digest of the full plan, six 8-byte counts in
discovered/selected/cached/executed/passed/failed order, 4-byte status, and 124 zero padding bytes.
Integers SHALL be unsigned little-endian, with exact unsigned 64-bit counts and checked arithmetic
without unsafe-number narrowing. The catalog digest SHALL be SHA-256 over catalog-order entries,
each framed by its unsigned 64-bit ordinal and UTF-8 identity byte length followed by identity
bytes, and SHALL NOT serve as a result-cache key. It MAY be computed incrementally.

The runner SHALL validate the complete compact plan, including exact size, magic/version/mode,
padding, and discovered count and digest matching its compiled catalog, before executing any test.
It SHALL retain file/name filtering over the full compiled catalog, sequential execution of all
selected tests, ordinary cleanup/recovery, streaming per-test output, and summary/status behavior.
Both modes SHALL compile every discovered test before filtering. The compact completion receipt
SHALL be written only after the loop and cleanup finish. Admission SHALL validate exact layout,
nonce, plan digest, counts, and process status: discovered equals the plan, selected does not
exceed discovered, cached is zero, executed equals selected, passed plus failed equals executed,
and status is 0 for no failures or 1 for test failures. Zero selection SHALL remain status 0.
Invalid compact plans or normally exited invalid receipts SHALL be operational status 2; abnormal
termination SHALL retain its meaning. Values outside existing compiler/runtime representability
limits SHALL fail operationally without wrapping, saturation, or cache-dependent test-count limits.

Compact mode MUST NOT carry execution identities, hit flags, or per-test dispositions, perform any
result-cache reads or writes, skip selected bodies, or publish pass records, even from a valid
successful aggregate receipt. The exchange size SHALL remain independent of identity length,
catalog size, selection size, and user output volume; the exchange bound SHALL NOT truncate output.

#### Scenario: Long declaration identity exceeds a per-test bound without hits

- **WHEN** a canonical declaration identity makes either per-test file exceed its bound even though no record is a cache hit
- **THEN** the workflow uses the fixed compact plan and aggregate receipt, every selected test executes normally, cached is zero, and no result records are read or published

#### Scenario: Large discovered catalog exceeds the hard ceiling

- **WHEN** enough discovered tests make a per-test file exceed 16 MiB or checked size preflight cannot establish a bound, even if a runtime filter selects few or no tests
- **THEN** the workflow uses the same 256-byte compact files, compiles all discovered tests, applies filters in the runner, and preserves exact counts and the ordinary zero-selection outcome without hits or publication

#### Scenario: Receipt alone exceeds its bound

- **WHEN** the per-test plan fits but the maximum complete receipt would exceed its bound
- **THEN** preflight selects compact uncached mode before result-cache lookup or any test executes, without attempting an oversized receipt or rerunning test bodies

#### Scenario: Compact completion preserves test failure semantics

- **WHEN** selected tests in compact mode complete with both passes and recovered test failures
- **THEN** each selected body runs once in catalog order with normal cleanup and output, the aggregate reports cached zero and exact executed/pass/fail counts, status is 1, and no passing result is published

#### Scenario: Reject invalid compact authority

- **WHEN** a compact plan has extra per-test data, invalid framing or padding, or a catalog count or digest mismatch
- **THEN** the runner rejects it before any test executes with operational status 2 rather than downgrading or accepting a hit

#### Scenario: Reject an invalid aggregate receipt

- **WHEN** a normally exiting compact run returns the wrong mode, size, padding, nonce, plan digest, counts, or status, or an incomplete receipt
- **THEN** the invocation exits with operational status 2 and publishes no results

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
