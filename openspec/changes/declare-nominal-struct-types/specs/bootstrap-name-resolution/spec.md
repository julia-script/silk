## ADDED Requirements

### Requirement: Declared nominal types resolve through explicit module scopes

Type positions in struct fields and function contracts SHALL resolve built-in scalar names, local
nominal declarations, selected nominal imports, and namespace-qualified public nominal members.
The lookup SHALL use the same immutable module scope, collision rules, canonical declaration
identities, and visibility outcomes as value-level declaration resolution while requiring the
resolved member to be a type declaration. It MUST NOT search filenames, infer imports, prefer a
declaration kind, or reinterpret a function as a type.

#### Scenario: Resolve a local nominal type

- **WHEN** a field or function contract names a unique local struct
- **THEN** the type lookup resolves that struct's canonical identity

#### Scenario: Resolve a selected nominal type

- **WHEN** a module imports `syntax.Tree { Node }` and a field names `Node`
- **THEN** the type lookup resolves the selected public struct's canonical identity

#### Scenario: Resolve a qualified nominal type

- **WHEN** a module imports `syntax.Tree as Tree` and a field names `Tree.Node`
- **THEN** the type lookup resolves the public struct through the namespace alias

#### Scenario: Refuse a function in type position

- **WHEN** a field type path resolves to a function declaration
- **THEN** the type remains unavailable with a kind-mismatch diagnostic and no alternate candidate

### Requirement: Nominal type visibility follows declaration boundaries

A nominal type SHALL be externally resolvable only when its struct declaration is public. A
qualified or selected lookup of a private struct SHALL retain the inaccessible candidate and exact
use-site provenance while producing no available type. Public declarations that expose nominal
types SHALL be validated after type lookup against the defining modules' visibility facts.

#### Scenario: Import a public struct

- **WHEN** a module selects a public struct from another module
- **THEN** the selected binding and every valid type use identify the same canonical nominal type

#### Scenario: Refuse a private struct

- **WHEN** a module selects or qualifies a private struct from another module
- **THEN** lookup retains its inaccessible identity and one visibility diagnostic without resolving a usable type

#### Scenario: Preserve unrelated lookups

- **WHEN** one nominal type lookup is inaccessible or conflicting
- **THEN** unrelated local and imported type lookups in the same closure remain available
