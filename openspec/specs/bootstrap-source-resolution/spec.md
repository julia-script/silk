# bootstrap-source-resolution Specification

## Purpose

Resolve canonical Silk module identities to exact source bytes through replaceable storage
implementations while keeping missing sources distinct from operational access failures.

## Requirements

### Requirement: Source resolution is a replaceable capability

The compiler SHALL request imported source bytes through a replaceable source-resolution
capability whose input is one canonical logical module identity. A successful resolution SHALL
return the exact source bytes for that identity, and consumers SHALL be able to provide at least an
in-memory implementation without depending on a host filesystem.

#### Scenario: Resolve from memory

- **WHEN** an in-memory resolver contains bytes for canonical identity `compiler/Syntax`
- **THEN** resolving `compiler/Syntax` returns those exact bytes without accessing a host filesystem

#### Scenario: Replace storage implementations

- **WHEN** browser tooling and the command-line compiler provide different resolver implementations for the same canonical module graph
- **THEN** closure loading observes the same module identities, source bytes, import facts, and diagnostics

### Requirement: Filesystem lookup is rooted and exact

A filesystem resolver SHALL map canonical identity `<module>` exactly to
`<source-root>/<module>.silk`. Lookup SHALL be relative to the compilation source root rather than
the importing module's directory, SHALL preserve the identity's case, and SHALL NOT probe alternate
extensions, directory indexes, parent directories, or case-folded names. The default source root
SHALL be the entry file's containing directory; an explicitly selected source root SHALL derive the
entry module identity from the entry's relative extensionless path.

#### Scenario: Resolve a nested module from the root

- **WHEN** the source root is `/project/src` and any module imports `compiler.Syntax`
- **THEN** the filesystem resolver requests exactly `/project/src/compiler/Syntax.silk`

#### Scenario: Ignore the importing directory

- **WHEN** module `app/features/Main` imports `compiler.Syntax`
- **THEN** resolution still requests `<source-root>/compiler/Syntax.silk` rather than a path below `app/features`

#### Scenario: Derive a nested entry identity

- **WHEN** `/project/src` is selected as the source root and the entry is `/project/src/app/Main.silk`
- **THEN** the root module identity is `app/Main`

### Requirement: Absence and operational failure remain distinct

Resolving a canonical identity that has no source SHALL return an ordinary absent outcome.
Permission errors, malformed resolver state, storage failures, and equivalent inability to decide
whether source exists SHALL fail with a typed source-resolution error carrying the requested module
identity, operation, message, and structured reason. An operational failure MUST NOT be reported as
an unknown-module source diagnostic.

#### Scenario: Report an absent source

- **WHEN** no source exists for `missing/Module`
- **THEN** resolution returns the absent outcome without a typed operational failure

#### Scenario: Preserve an unreadable-source failure

- **WHEN** `compiler/Syntax.silk` exists but the resolver cannot read it
- **THEN** resolution fails with a typed error naming `compiler/Syntax` and retains the underlying failure as diagnostic ancestry

### Requirement: Resolution is deterministic within one compilation

Each canonical imported module identity SHALL be resolved at most once during one closure load,
including identities whose resolution is absent or fails. Repeated import sites SHALL reuse that
recorded outcome, and resolution results and failure ordering SHALL be canonical and independent of
import traversal or filesystem enumeration order.

#### Scenario: Share one resolution across a diamond

- **WHEN** two reachable modules import the same canonical target
- **THEN** the resolver is invoked once for that target and both import facts observe the same outcome

#### Scenario: Order several resolver failures

- **WHEN** several reachable imports fail operationally under different traversal orders
- **THEN** the recorded failure sequence is ordered by canonical module identity in every run

### Requirement: The CLI distinguishes source rejection from operational failure

The command-line compiler SHALL render diagnostics against every loaded source and SHALL distinguish
successful compilation, source rejection, and operational resolution failure. Success SHALL exit
with status `0`, source rejection SHALL exit with status `1`, and a source-resolution failure SHALL
exit with status `2`; neither failing outcome may leave the requested output artifact committed.

#### Scenario: Reject a missing imported module

- **WHEN** a source imports an absent canonical module
- **THEN** the CLI renders the import diagnostic, exits with status `1`, and commits no output artifact

#### Scenario: Fail on an unreadable imported module

- **WHEN** an imported module cannot be read because of an operational filesystem failure
- **THEN** the CLI renders the operational failure, exits with status `2`, and commits no output artifact

### Requirement: Standard-library resolution preserves toolchain source origin

Source resolution SHALL distinguish project-owned identities from reserved standard-library
identities before filesystem lookup. A standard-library identity SHALL resolve through the
toolchain manifest and source root, returning exact bytes plus its canonical physical source
location. It MUST NOT be fabricated below the project source root or shadowed by a coincidentally
named project file.

#### Scenario: Resolve a standard-library file

- **WHEN** a project imports `silk/vector`
- **THEN** resolution returns the exact shipped file and location recorded by the toolchain manifest rather than `<project-root>/silk/vector.silk`

#### Scenario: Report a missing packaged source

- **WHEN** the manifest names a module whose canonical file is absent
- **THEN** resolution returns a typed toolchain-source failure without falling back to project lookup or unrelated embedded bytes

#### Scenario: Preserve in-memory tooling

- **WHEN** browser tooling supplies the manifest's exact source bytes without a host filesystem
- **THEN** ordinary module analysis observes the same canonical identity and source contents while the source origin remains explicitly non-file-backed
