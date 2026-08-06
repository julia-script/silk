## Purpose

Define nominal bootstrap struct types as canonical, visible, fully typed declarations whose field
dependencies and target-aware physical layouts are deterministic compiler facts.

## ADDED Requirements

### Requirement: Struct declarations create nominal types

Each present top-level struct declaration SHALL create one nominal type identified by its canonical
module identity and declaration name. A struct SHALL retain its explicit public or default-private
visibility and its fields in physical declaration order. Every field SHALL retain its name, exact
type syntax, explicit public or default-private visibility, and source provenance. Struct identity
MUST NOT depend on field shape, import spelling, declaration traversal order, or target layout.

#### Scenario: Distinguish equal shapes in different modules

- **WHEN** two modules each declare `pub struct Position { x: I32 }`
- **THEN** the compiler records two distinct nominal types even though their names and fields match

#### Scenario: Declare a zero-field marker

- **WHEN** a module declares `pub struct End {}`
- **THEN** the compiler records one available nominal marker type with an empty ordered field list

#### Scenario: Preserve field order

- **WHEN** a struct declares fields `first`, `second`, and `third` in that order
- **THEN** its semantic field facts and logical cleanup order retain exactly that declaration order

### Requirement: Struct fields are explicit and unique

Every struct field SHALL have one present name and one explicit resolved type. The first present
occurrence of a field name SHALL own that field identity; later occurrences SHALL remain explicit
duplicates with a stable diagnostic cause and MUST NOT replace the first field. Missing names,
missing types, and unknown types SHALL remain unavailable without fabricating field facts.

#### Scenario: Resolve scalar fields

- **WHEN** a struct declares `count: I32` and `ready: Bool`
- **THEN** both fields resolve to their exact scalar types in declaration order

#### Scenario: Reject a duplicate field

- **WHEN** a struct declares `value: I32` twice
- **THEN** the first field remains available and the second remains an explicit duplicate caused by one semantic diagnostic

#### Scenario: Preserve a damaged field

- **WHEN** a field is missing its name or declared type
- **THEN** that field remains explicitly unavailable while later fields and declarations continue to resolve

### Requirement: Struct visibility protects nominal boundaries

Top-level structs and their fields SHALL be private by default; one explicit `pub` SHALL make that
struct or field accessible from importing modules. A public function contract, public struct field,
or other externally nameable public declaration MUST NOT expose a private nominal type. Private
fields MAY use private types. Visibility failure SHALL preserve the referenced type identity and
produce one diagnostic at the exposing type use rather than pretending the type is unknown.

#### Scenario: Keep default-private fields inaccessible

- **WHEN** another module imports a public struct containing one default-private and one public field
- **THEN** the struct and public field are externally nameable while the private field remains inaccessible

#### Scenario: Reject a public field exposing a private type

- **WHEN** a public struct has a public field whose resolved type is a private struct
- **THEN** the field retains that type dependency but is unavailable because its public contract leaks a private type

#### Scenario: Allow a private representation type

- **WHEN** a public struct has a private field whose type is another private struct in the same module
- **THEN** both declarations resolve without a visibility diagnostic

### Requirement: Inline struct dependencies are finite

Resolved nominal fields SHALL form a deterministic type-dependency graph. Acyclic dependencies
SHALL be available regardless of source or module order. Any direct or mutual cycle consisting only
of inline struct fields SHALL make every participating struct's layout unavailable with one stable,
canonically attributed diagnostic cycle. The compiler MUST NOT guess a size, silently add
indirection, or reject an import cycle that contains no inline type cycle.

#### Scenario: Resolve an acyclic nested struct

- **WHEN** `Span` contains two `Position` fields and `Position` contains scalar fields
- **THEN** both nominal types and their dependency edge are available independently of declaration order

#### Scenario: Reject direct inline recursion

- **WHEN** `Node` declares a field of type `Node`
- **THEN** `Node` retains its nominal identity and fields but its dependency and layout state identify the direct recursive cycle as unavailable

#### Scenario: Reject mutual inline recursion across modules

- **WHEN** `syntax.Expression` contains `statement.Statement` and `statement.Statement` contains `syntax.Expression`
- **THEN** both nominal identities remain queryable and one canonical cycle makes both layouts unavailable

#### Scenario: Ignore a harmless module cycle

- **WHEN** two cyclically importing modules declare structs whose field dependency graph is acyclic
- **THEN** their struct facts and layouts remain available without a diagnostic attributed solely to the import cycle
