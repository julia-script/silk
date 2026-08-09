## ADDED Requirements

### Requirement: Project documentation generation

The root `silk` command SHALL expose `doc` as a project-oriented workflow. `silk doc` SHALL analyze
the reachable project source closure without invoking a backend, linker, or program execution and
SHALL write deterministic experimental Silk documentation JSON to an explicit or deterministic
default destination. It SHALL include only public declarations by default and SHALL accept an
explicit option to include private declarations.

#### Scenario: Generate public documentation

- **WHEN** a valid project containing documented public and private declarations runs `silk doc`
- **THEN** the command writes deterministic JSON containing the public declarations and omitting the private declarations

#### Scenario: Generate private documentation explicitly

- **WHEN** the same project runs `silk doc --include-private`
- **THEN** the JSON contains both public and private declarations with their visibility

#### Scenario: Refuse source damage

- **WHEN** the reachable project source closure contains compiler diagnostics that make semantic documentation facts unavailable
- **THEN** `silk doc` reports the diagnostics, writes no partial destination, and exits with the existing source-rejection class
