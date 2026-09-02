# bootstrap-struct-types Specification

## Purpose

Define nominal bootstrap struct types as canonical, visible, fully typed declarations whose field
dependencies and target-aware physical layouts are deterministic compiler facts.

## Requirements

### Requirement: Struct declarations create nominal types

Each present top-level struct declaration SHALL create one nominal type identified by its canonical
module identity and declaration name. A struct SHALL retain its explicit public or default-private
visibility and its fields in physical declaration order. Every field SHALL retain its name, exact
type syntax, explicit public or default-private visibility, and source provenance. Struct identity
MUST NOT depend on field shape, import spelling, declaration traversal order, or target layout.

#### Scenario: Distinguish equal shapes in different modules

- **WHEN** two modules each declare `pub struct Position { x: i32 }`
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

- **WHEN** a struct declares `count: i32` and `ready: bool`
- **THEN** both fields resolve to their exact scalar types in declaration order

#### Scenario: Reject a duplicate field

- **WHEN** a struct declares `value: i32` twice
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

### Requirement: Inline struct dependencies are finite

Resolved nominal fields SHALL form a deterministic type-dependency graph. A field SHALL retain every
nominal type it references as a reported dependency. Cycle detection SHALL use the narrower graph of
_inline reach_: the nominals whose layout a field's layout actually requires.

Inline reach SHALL be defined as follows. A field reaches a nominal inline when the nominal appears
in the field's type outside every indirecting position. A type argument of a compiler-owned
indirection whose layout is independent of that argument SHALL NOT be an inline position. A type
argument of a declared generic SHALL be an inline position exactly when that generic reaches its own
corresponding type parameter inline, computed as a monotone fixed point over the declarations and
therefore independent of declaration and module order.

Acyclic inline dependencies SHALL be available regardless of source or module order. Any direct or
mutual cycle consisting only of inline struct fields SHALL make every participating struct's layout
unavailable with one stable, canonically attributed diagnostic cycle. A cycle that passes through at
least one indirecting position SHALL be available, because the participating layouts are finite. The
compiler MUST NOT guess a size, silently add indirection, or reject an import cycle that contains no
inline type cycle.

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

#### Scenario: Accept a cycle that passes through explicit indirection

- **WHEN** a struct declares a field whose type reaches the struct itself only behind an explicit heap indirection
- **THEN** the struct's dependency state is available, its layout is finite, and no cycle diagnostic is produced

#### Scenario: Reject a cycle through a generic that inlines its parameter

- **WHEN** `Wrapper` holds its type parameter as an inline field and `Node` declares a field of type `Wrapper<Node>`
- **THEN** the cycle remains inline throughout and `Node` is rejected exactly as a direct self-reference is

#### Scenario: Accept a cycle through a generic that indirects its parameter

- **WHEN** a generic reaches its type parameter only through an indirecting position and a struct declares a field of that generic applied to itself
- **THEN** the struct is available, without the compiler recognizing the generic by name

#### Scenario: Report a reference behind indirection as a dependency

- **WHEN** a field's type mentions a nominal only inside an indirecting position
- **THEN** the field still reports that nominal as a dependency while contributing no cycle edge

### Requirement: Aggregate declarations publish authorized static reflection order

Every concrete nominal aggregate SHALL publish a deterministic reflection kind and declaration
order derived from its existing canonical struct representation. Named tuples and anonymous
positional aggregates SHALL publish ordered positions. Named structs and anonymous named aggregates
SHALL publish ordered labels. Each reflected member SHALL retain its concrete specialized field type
and existing visibility authority without inventing structural compatibility or source-visible
synthetic tuple fields.

#### Scenario: Reflect a named tuple without synthetic labels

- **WHEN** `tuple Point(u32, u32)` is reflected
- **THEN** its descriptor contains positions zero and one with type `u32` and exposes no `_0`, `_1`, or other generated field spelling

#### Scenario: Preserve source order and visibility

- **WHEN** a named struct has public and private fields in declaration order
- **THEN** authorized reflection preserves the relative order of visible public fields while revealing no inaccessible field name

### Requirement: Positional and anonymous aggregates have canonical nominal declarations

Each named tuple declaration SHALL enter the nominal declaration catalog with ordered synthesized
position identities and explicit element types. Each uncontextualized tuple or record literal SHALL
enter the semantic catalog as one compiler-synthesized nominal struct declaration keyed by its
canonical module and source occurrence. Synthesized identities SHALL be stable across fresh
processes and MUST NOT depend on inferred member shape, source traversal order, cache state, target,
or backend layout.

Synthesized declarations SHALL remain semantic facts rather than source declarations: they MUST NOT
introduce a spelling into lexical lookup, imports, exports, hover text pretending that the user
declared a name, or compiler-recognized standard-library names. Their ordered positions or fields
SHALL otherwise participate in the same finite-layout validation as source structs.

#### Scenario: Distinguish two generated declarations

- **WHEN** one module contains two separate same-shaped anonymous record literals
- **THEN** the declaration catalog records two deterministic nominal identities tied to their separate source occurrences

#### Scenario: Keep a generated declaration unnameable

- **WHEN** tooling and name lookup inspect an anonymous aggregate
- **THEN** they expose its source occurrence and members without adding any identifier that source can import or write
