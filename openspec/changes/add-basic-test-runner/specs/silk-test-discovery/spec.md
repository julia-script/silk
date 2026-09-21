## Purpose

Expose a deterministic static catalog of project tests and exact typed callable access so ordinary Silk can implement test runners.

## ADDED Requirements

### Requirement: Discovery uses an explicit root and project ownership

A test-discovery request SHALL identify one canonical discovery root and the requesting project's authoritative source ownership independently of the runner/application and runtime roots. The candidate set SHALL consist exactly of active test declarations belonging to that project in the discovery root's transitive active import closure. Dependencies SHALL still participate in ordinary compilation, but dependency-owned and toolchain-owned tests SHALL be excluded. Files outside that closure and modules reachable only from runner/runtime composition SHALL NOT become candidates. Source ownership SHALL NOT be inferred from incidental physical path prefixes. Missing required roots SHALL follow existing required-root failures.

#### Scenario: Import a separate test file

- **WHEN** an import-only test root imports a project module containing private tests without calling its functions
- **THEN** those tests are discovered without explicit registration or a `main` in the discovery root

#### Scenario: Exclude neighboring and dependency tests

- **WHEN** an unimported project file contains tests and a reachable dependency contains tests
- **THEN** neither contributes a catalog entry

#### Scenario: Exclude runner-only imports

- **WHEN** runner composition loads a project support module that is not reachable from the discovery root
- **THEN** its tests are excluded even though the module belongs to the complete compilation

#### Scenario: Use selected import edges

- **WHEN** a test file is imported only by an inactive module branch
- **THEN** that import does not load the file or discover its tests

### Requirement: A finite static catalog preserves declaration identity

`Intrinsic.tests()` SHALL return a finite heterogeneous static iterable of sealed `Intrinsic.Test<F>` descriptors, with one entry per eligible canonical declaration and its exact callable type `F`. Results SHALL be ordered by canonical module identity, declared test name, and canonical authored owner identity. Multiple import paths or cycles SHALL NOT duplicate entries. A query without discovery context, during configuration/module selection, or without a valid selected declaration surface SHALL be rejected; it SHALL NOT guess a root or silently return an empty catalog. Catalog membership and observed metadata SHALL participate in static invalidation and compilation identity.

#### Scenario: Deduplicate a diamond graph

- **WHEN** two imported modules reach the same test module
- **THEN** each test in that module appears once in deterministic catalog order

#### Scenario: Refresh private test membership

- **WHEN** a revision adds or removes a private test without changing the module's ordinary public surface
- **THEN** the next catalog reflects the new membership rather than reusing stale discovery results

#### Scenario: Reject circular selection

- **WHEN** a module-selection condition queries tests to decide which test module to import
- **THEN** analysis rejects the query in that phase rather than provisionally choosing a discovery graph

### Requirement: Test metadata separates identity presentation and content

`Intrinsic.testInfo` SHALL accept a static test descriptor and produce ordinary immutable metadata values containing an opaque canonical declaration identity, declared name, canonical module identity, logical project-relative source path, current declaration line and column, and an opaque versioned authored-HIR fingerprint. Source coordinates SHALL be one-based and follow the existing source-position convention. Logical path metadata SHALL be supplied by the request/source mapping, including in-memory sources, without requiring an ambient filesystem read. Moving a declaration SHALL update its location without confusing location, declaration identity, and authored content.

#### Scenario: Move a test without editing its content

- **WHEN** whitespace or a differently named declaration is inserted before a test
- **THEN** metadata reports its current location while preserving its authored fingerprint and stable owner identity

#### Scenario: Preserve separate identities

- **WHEN** two modules contain tests with identical declared names and authored content
- **THEN** their metadata retains distinct declaration identities and module/source paths even if the fingerprints are equal

### Requirement: Callable extraction preserves exact typing and erases discovery descriptors

`Intrinsic.testFunction` SHALL consume a required static test descriptor during specialization and produce the referenced ordinary runtime callable of exact type `F`, preserving unit result or Effect success, failure, and requirement types. Only descriptors issued for eligible marked declarations SHALL authorize private test access. Descriptors and catalog values SHALL be phase-only, unforgeable, and absent from residual signatures, runtime storage, TIR/MIR executable operations, and backend reflection inventories. Static iteration SHALL obey existing finite budgets and atomic expansion rules. Extracted ordinary metadata constants and callable values SHALL be usable at runtime without a test-specific invocation intrinsic.

#### Scenario: Specialize heterogeneous failures

- **WHEN** source iterates a catalog containing two Effect tests with different failure types
- **THEN** each iteration receives its exact callable type and can invoke ordinary generic recovery without erasing the failure channel

#### Scenario: Reject a runtime descriptor

- **WHEN** source attempts to store a test descriptor in a runtime field or pass it through a residual argument
- **THEN** analysis reports a phase violation and publishes no executable descriptor representation

#### Scenario: Use another source runner

- **WHEN** an explicitly configured ordinary Silk application with a different module/name consumes the same discovery context and intrinsics
- **THEN** it can inspect and call the same tests without compiler registration of that runner's spelling

### Requirement: Fingerprints describe local authored content only

The test fingerprint SHALL be a domain-separated versioned SHA-256 digest over framed canonical authored header and body encodings. It SHALL exclude presentation, raw source trivia, unrelated pool contents, physical paths, runtime addresses, and compilation-session identity. Declaration identity SHALL remain separate. The fingerprint SHALL NOT claim to cover called implementations, external resolved meanings, providers, target/profile/compiler/runner changes, or external runtime inputs. A catalog consumer observing fingerprints SHALL be invalidated when the corresponding authored content changes even if ordinary public signatures remain equal.

#### Scenario: Ignore presentation-only edits

- **WHEN** only comments, whitespace, current source offsets, or unrelated pool numbering change
- **THEN** the unchanged test's fingerprint remains equal

#### Scenario: Include both contract and implementation

- **WHEN** a test's authored header or body changes
- **THEN** the digest input changes and the catalog publishes the fingerprint of the new canonical content

#### Scenario: Do not mistake a helper edit for unchanged behavior

- **WHEN** only a function called by a test changes implementation
- **THEN** the test's local authored fingerprint can remain equal and provides no authorization to reuse its previous test result
