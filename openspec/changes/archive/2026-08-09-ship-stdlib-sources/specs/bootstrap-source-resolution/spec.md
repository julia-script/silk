## ADDED Requirements

### Requirement: Standard-library resolution preserves toolchain source origin

Source resolution SHALL distinguish project-owned identities from reserved standard-library identities before filesystem lookup. A standard-library identity SHALL resolve through the toolchain manifest and source root, returning exact bytes plus its canonical physical source location. It MUST NOT be fabricated below the project source root or shadowed by a coincidentally named project file.

#### Scenario: Resolve a standard-library file

- **WHEN** a project imports `silk/vector`
- **THEN** resolution returns the exact shipped file and location recorded by the toolchain manifest rather than `<project-root>/silk/vector.silk`

#### Scenario: Report a missing packaged source

- **WHEN** the manifest names a module whose canonical file is absent
- **THEN** resolution returns a typed toolchain-source failure without falling back to project lookup or unrelated embedded bytes

#### Scenario: Preserve in-memory tooling

- **WHEN** browser tooling supplies the manifest's exact source bytes without a host filesystem
- **THEN** ordinary module analysis observes the same canonical identity and source contents while the source origin remains explicitly non-file-backed
