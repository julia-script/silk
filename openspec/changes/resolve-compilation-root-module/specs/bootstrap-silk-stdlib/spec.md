## MODIFIED Requirements

### Requirement: Standard-library modules resolve without vendoring

The compiler SHALL ship canonical standard-library `.silk` source files and resolve their module
identities when importing programs do not contain those sources. Standard-library module identities
and source roots SHALL be canonical, disjoint from user module identities, and stable across
processes, hosts, compilation orders, and supported package installation layouts.

#### Scenario: Import a library module from user source

- **WHEN** a user program imports a standard-library module that is not present in the user's source set
- **THEN** resolution succeeds through the ordinary module closure and the resolved declarations carry the library's canonical module identity

#### Scenario: User modules cannot collide with library identity

- **WHEN** a project source supply or editor overlay contains bytes under a reserved standard-library identity, including a requested root
- **THEN** the compiler resolves that identity exclusively through the toolchain source capability and never consults the project bytes

#### Scenario: Library resolution is deterministic

- **WHEN** the same program importing standard-library modules is compiled in two fresh processes
- **THEN** every published artifact that mentions library declarations is byte-identical

#### Scenario: Package the standard library

- **WHEN** a compiler package or toolchain distribution is assembled
- **THEN** content verification finds every canonical `.silk` file required by its deterministic manifest
