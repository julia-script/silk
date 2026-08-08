# bootstrap-silk-stdlib Specification

## Purpose

Define how Silk standard-library modules ship with the compiler and become reachable from user
programs without vendoring source, while compiling through the same pipeline with no privilege.

## Requirements

### Requirement: Standard-library modules resolve without vendoring

The compiler SHALL ship a standard-library module set and resolve imports of its modules when the
importing program does not contain their source. Standard-library module identities SHALL be
canonical, disjoint from user module identities, and stable across processes, hosts, and compilation
orders.

#### Scenario: Import a library module from user source

- **WHEN** a user program imports a standard-library module that is not present in the user's source set
- **THEN** resolution succeeds through the ordinary module closure and the resolved declarations carry the library's canonical module identity

#### Scenario: User modules cannot collide with library identity

- **WHEN** a user program declares a module whose name would shadow a standard-library module
- **THEN** the compiler reports a deterministic diagnostic naming both origins instead of silently preferring either

#### Scenario: Library resolution is deterministic

- **WHEN** the same program importing standard-library modules is compiled in two fresh processes
- **THEN** every published artifact that mentions library declarations is byte-identical

### Requirement: Standard-library code has no compiler privilege

Standard-library modules SHALL be ordinary Silk source compiled through the same lexer, parser,
elaboration, ownership, and lowering as user code, with the same diagnostics and no phase branching
on library origin. A defect in library source SHALL surface as ordinary diagnostics attributed to
the library module.

#### Scenario: Library source fails like user source

- **WHEN** a standard-library module contains a semantic error
- **THEN** compilation reports the same diagnostic an identical user module would receive, attributed to the library module's canonical identity

#### Scenario: No library-origin branch in published artifacts

- **WHEN** semantic, HIR, ownership, or MIR artifacts for a library declaration are inspected
- **THEN** they use the same canonical forms as user declarations with no library-kind tag
