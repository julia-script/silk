## ADDED Requirements

### Requirement: Project analysis revises an accepted immutable project

Project frontend analysis SHALL accept an optional prior completed project analysis as a reuse
basis. The resulting project SHALL be a new immutable coherent analysis over the current roots and
resolved closure; the prior project SHALL remain unchanged.

#### Scenario: Revise one edited root

- **WHEN** one root changes and a prior completed multi-root project is supplied
- **THEN** analysis returns a complete new project whose root views all describe the current revision

### Requirement: Unchanged module syntax is reused exactly

When a current module has the same canonical module identity, source origin, and byte sequence as a
module in the prior project, closure loading SHALL reuse the exact immutable syntax artifact rather
than lexing and parsing it again. A changed origin or byte sequence MUST produce a new syntax
artifact.

#### Scenario: Edit one module beside an unchanged dependency

- **WHEN** a root changes but a resolved dependency retains its identity, origin, and bytes
- **THEN** the new project references the dependency's prior syntax artifact by object identity and parses a new root syntax artifact

#### Scenario: Preserve equal bytes from a changed origin

- **WHEN** a module keeps equal bytes but its source origin changes
- **THEN** the new project constructs a fresh syntax artifact rather than reusing origin-owned spans and diagnostics

### Requirement: Project syntax revision evidence is explicit

Each current project module SHALL have exactly one immutable syntax revision observation identifying
it as fresh, exactly reused, or reparsed with an adjacent-revision correspondence. Removed prior
modules SHALL not appear as current observations. Reparsed modules with no same-identity predecessor
SHALL be fresh.

#### Scenario: Observe a mixed revision

- **WHEN** a project revision contains one unchanged module, one edited module, and one newly resolved module
- **THEN** its observations identify the modules as reused, changed with correspondence, and fresh respectively

### Requirement: Syntax reuse does not reuse semantic facts prematurely

Project revision analysis SHALL recompute declaration, resolution, elaboration, ownership, tooling,
diagnostic, and root-view facts for the complete current closure even when some syntax artifacts are
reused. It MUST NOT expose semantic facts from a prior project as current merely because syntax was
reused or corresponded.

#### Scenario: Recompute one coherent frontend

- **WHEN** any module changes while other module syntax is reused
- **THEN** every current root view shares one newly completed semantic frontend and no prior semantic index is exposed as current
