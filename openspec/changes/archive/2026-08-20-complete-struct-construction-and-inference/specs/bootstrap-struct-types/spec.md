## MODIFIED Requirements

### Requirement: Struct visibility protects nominal boundaries

Top-level structs and their fields SHALL be private by default; one explicit `pub` SHALL make that
struct or field accessible from importing modules. A public function contract, public struct field,
or other externally nameable public declaration MUST NOT expose a private nominal type. Private
fields MAY use private types. Visibility failure SHALL preserve the referenced type identity and
produce one diagnostic at the exposing type use rather than pretending the type is unknown.

For source-declared structs, field visibility SHALL be the raw-construction boundary. A public
struct whose required fields are all public MAY be constructed from another module. Any private
required field SHALL retain construction control for the defining module without changing the
visibility of the type or its other fields. A nominal type that has no source struct declaration,
such as an opaque runtime handle, SHALL NOT become constructible merely because its semantic type
fact exposes no fields.

#### Scenario: Keep default-private fields inaccessible

- **WHEN** another module imports a public struct containing one default-private and one public field
- **THEN** the struct and public field are externally nameable while the private field remains inaccessible

#### Scenario: Reject a public field exposing a private type

- **WHEN** a public struct has a public field whose resolved type is a private struct
- **THEN** the field retains that type dependency but is unavailable because its public contract leaks a private type

#### Scenario: Allow a private representation type

- **WHEN** a public struct has a private field whose type is another private struct in the same module
- **THEN** both declarations resolve without a visibility diagnostic

#### Scenario: Use a private field as the construction fence

- **WHEN** another module can name a public struct but one required field is private
- **THEN** raw construction remains unavailable while visible factory functions remain callable

#### Scenario: Preserve a declarationless opaque type

- **WHEN** a public nominal runtime type has semantic type facts but no source struct declaration
- **THEN** raw construction remains unavailable even though no source fields are visible
