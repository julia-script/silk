## MODIFIED Requirements

### Requirement: Source resolution is a replaceable capability

The compiler SHALL request root and imported source bytes through a replaceable source-resolution
capability whose input is one canonical logical module identity. A successful resolution SHALL
return the exact source bytes for that identity, and consumers SHALL be able to provide at least an
in-memory implementation without depending on a host filesystem.

#### Scenario: Resolve from memory

- **WHEN** an in-memory resolver contains bytes for canonical identity `compiler/Syntax`
- **THEN** resolving `compiler/Syntax` returns those exact bytes without accessing a host filesystem

#### Scenario: Replace storage implementations

- **WHEN** browser tooling and the command-line compiler provide different resolver implementations for the same canonical module graph
- **THEN** closure loading observes the same module identities, source bytes, import facts, and diagnostics

#### Scenario: Preserve root source origin

- **WHEN** the resolver supplies a root with a file-backed or in-memory origin
- **THEN** the analysis exposes that exact origin for root syntax and diagnostics

#### Scenario: Resolve a reserved root

- **WHEN** a requested root has a reserved standard-library identity
- **THEN** the root uses toolchain resolution and cannot be shadowed by project or editor-overlay bytes

### Requirement: Resolution is deterministic within one compilation

Each canonical root or imported module identity SHALL be resolved at most once during one closure load,
including identities whose resolution is absent or fails. Repeated import sites SHALL reuse that
recorded outcome, and resolution results and failure ordering SHALL be canonical and independent of
import traversal or filesystem enumeration order.

#### Scenario: Share one resolution across a diamond

- **WHEN** two reachable modules import the same canonical target
- **THEN** the resolver is invoked once for that target and both import facts observe the same outcome

#### Scenario: Order several resolver failures

- **WHEN** several reachable imports fail operationally under different traversal orders
- **THEN** the recorded failure sequence is ordered by canonical module identity in every run

#### Scenario: Share a root with another root or import

- **WHEN** the same identity is named as an application, project, or composition root and is also imported
- **THEN** one closure load requests it at most once and every use observes the same bytes and origin

### Requirement: The CLI distinguishes source rejection from operational failure

The command-line compiler SHALL render diagnostics against every loaded source and SHALL distinguish
successful compilation, source rejection, and operational resolution failure. Success SHALL exit
with status `0`, source rejection SHALL exit with status `1`, and a source-resolution failure SHALL
exit with status `2`; neither failing outcome may leave the requested output artifact committed. An unavailable explicitly
requested root SHALL be reported as a request/operational failure with status `2`, including when
its source is absent, without fabricating a source diagnostic.

#### Scenario: Reject a missing imported module

- **WHEN** a source imports an absent canonical module
- **THEN** the CLI renders the import diagnostic, exits with status `1`, and commits no output artifact

#### Scenario: Fail on an unreadable imported module

- **WHEN** an imported module cannot be read because of an operational filesystem failure
- **THEN** the CLI renders the operational failure, exits with status `2`, and commits no output artifact

#### Scenario: Fail on an unavailable root

- **WHEN** the selected entry identity resolves as absent or fails operationally inside frontend analysis
- **THEN** the CLI renders the typed root failure, exits with status `2`, and commits no output artifact
