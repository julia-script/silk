## ADDED Requirements

### Requirement: Test configuration selects bounded roots and one runner

The manifest SHALL accept an optional `[test]` table. When present, `roots` SHALL be a nonempty
ordered array of manifest-relative `.silk` paths and `runner`, when present, SHALL be one
manifest-relative `.silk` path. Every path SHALL resolve against the manifest directory, SHALL be
contained by the package's existing source root, and SHALL derive canonical module identity
relative to that source root. Repeated identical root paths SHALL remain in Project facts in their
declared order while compilation SHALL load their canonical module only once. When `[test]` is absent,
`package.root` SHALL be the sole user test root. An absent `runner` SHALL select the canonical
toolchain-owned `silk/test_runner`; user source MUST NOT shadow that shipped identity. The runner
SHALL remain a separately designated role and MAY name a path
also present in `roots`; it SHALL enter inventory membership only through that explicit test-root
role.

#### Scenario: Default to the package root and standard runner

- **WHEN** a user manifest has no `[test]` table
- **THEN** testing uses `package.root` as its sole test root and the canonical standard runner as the executable root

#### Scenario: Load several explicit test roots

- **WHEN** `[test]` declares two source-root-contained roots
- **THEN** both paths retain their manifest order for the compilation request while inventory order remains canonical and independent of that order

#### Scenario: Preserve repeated roots without repeated loading

- **WHEN** `[test].roots` repeats one identical manifest-relative path
- **THEN** Project facts retain both declared positions while the canonical module loads once and contributes each eligible declaration once

#### Scenario: Select an ordinary custom runner

- **WHEN** `[test].runner` names a source-root-contained Silk file with a valid zero-parameter main
- **THEN** that module becomes the distinct executable root and contributes no inventory entry unless it is also declared in `roots`

#### Scenario: Reject invalid test paths

- **WHEN** roots is empty, a test path is not an exact `.silk` file, or any root or runner escapes the source root
- **THEN** manifest loading fails with a typed project error identifying the invalid field and path reason

#### Scenario: Resolve paths from the manifest directory

- **WHEN** a manifest under `/project` has `source-root = "src"` and test root `src/tests/random.silk`
- **THEN** the root resolves as `/project/src/tests/random.silk` and its module identity is derived relative to `/project/src`

### Requirement: Standard-library tests use a deterministic toolchain catalog

The standard-library test target SHALL obtain its explicit canonical test roots from a versioned
deterministic toolchain catalog rather than a user manifest or directory scan. Catalog entries
SHALL obey the same canonical module identity, de-duplication, eligibility, and ordering rules as
user roots. The catalog and shipped-source table SHALL be generated and integrity-gated together;
a missing, corrupt, or mismatched catalog SHALL be an operational toolchain failure.

#### Scenario: Load the standard-library catalog repeatedly

- **WHEN** the same toolchain version runs its standard-library test target more than once
- **THEN** it supplies the identical ordered root set and obtains the identical canonical inventory

#### Scenario: Exclude an uncataloged standard-library file

- **WHEN** a standard-library source or test file is neither cataloged nor reachable from a cataloged root
- **THEN** it is not scanned or added to the test inventory

#### Scenario: Reject catalog drift

- **WHEN** the standard-library catalog is missing, corrupt, or does not match the shipped-source table
- **THEN** the standard-library test target fails operationally before analysis and invokes no runner
