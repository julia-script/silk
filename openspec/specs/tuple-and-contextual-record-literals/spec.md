# Tuple And Contextual Record Literals Specification

## Purpose

Define compact positional and named aggregate literals that preserve Silk's nominal type system by
resolving contextually or receiving stable source-occurrence identities.

## Requirements

### Requirement: Named tuples are nominal positional structs

A `tuple Name(T0, T1, ...)` declaration SHALL create one nominal aggregate type identified by its
canonical module and declaration identity. Its elements SHALL have canonical ordinal identities in
declaration order and SHALL use ordinary struct representation, ownership, layout, and cleanup.
Equal element types or equal runtime layouts MUST NOT make separately declared tuple types
compatible.

Named tuple values SHALL be constructed positionally as `Name(value0, value1, ...)` and projected
positionally as `value.0`, `value.1`, and so on. Synthesized internal field identities MUST NOT
become source field labels, so `Name { _0: value0 }` and other named-field construction spellings
SHALL be rejected. A stable explicit tuple type SHALL require a named declaration; Silk SHALL NOT
add a structural tuple type spelling such as `(T0, T1)`.

#### Scenario: Construct and project a named tuple

- **WHEN** `tuple Point(u32, u32)` is constructed as `Point(3, 4)` and its second element is projected
- **THEN** the value has canonical type `Point`, the projection resolves ordinal `1`, and its result has type `u32`

#### Scenario: Keep equal-shaped tuple declarations distinct

- **WHEN** two modules declare tuple types with the same element types
- **THEN** their values remain nominally incompatible despite sharing positional shape and runtime layout

#### Scenario: Hide synthesized struct fields

- **WHEN** source attempts to construct `Point` with `_0` and `_1` field labels
- **THEN** analysis rejects named-field construction and retains `Point(3, 4)` as its public construction form

### Requirement: Positional literals resolve from context or create one anonymous type

A positional tuple literal with at least two elements, or with one element followed by a trailing
comma, SHALL evaluate its elements left to right. When its expected type is a named tuple of equal
arity, each element SHALL be checked against the corresponding declared position and the result
SHALL have that expected nominal type. `()` SHALL remain the unit value, and `(value)` without a
trailing comma SHALL remain a parenthesized expression.

Without an expected named tuple type, one positional literal occurrence SHALL introduce one
anonymous nominal tuple-backed struct whose canonical identity is derived from the containing
module and source occurrence. Its position types SHALL be inferred from its element expressions.
Repeated evaluation of that syntax occurrence SHALL retain the same type identity; a distinct
literal occurrence SHALL receive a distinct identity even when every element type matches.

#### Scenario: Use an expected named tuple

- **WHEN** `let origin: Point = (0, 0)` is analyzed for `tuple Point(u32, u32)`
- **THEN** the literal constructs `Point` directly and introduces no anonymous aggregate type

#### Scenario: Infer one local anonymous tuple

- **WHEN** `let args = ("Julia", 32)` has no expected aggregate type and is passed to a generic function
- **THEN** the binding and generic argument retain one source-stable anonymous nominal type with `string` and integer positions

#### Scenario: Distinguish grouping from a one-element tuple

- **WHEN** source contains `(value)` and `(value,)`
- **THEN** the first is the ordinary parenthesized value and the second is a one-element tuple literal

### Requirement: Record literals contextually construct structs or create anonymous records

A record literal `.{ field0: value0, field1: value1 }` SHALL evaluate initializers in source order.
When an expected source-declared struct type exists, the literal SHALL use that nominal type's
ordinary construction authority, field visibility, completeness, generic inference, and
compatibility rules without requiring the caller to spell or import the type name at the literal.
Field order in the literal MUST NOT change the expected struct's canonical declaration order.

Without an expected struct type, one record literal occurrence SHALL introduce one anonymous
nominal struct. Its fields SHALL have the literal's unique labels, inferred types, and source order
as canonical declaration order. Its canonical identity SHALL derive from the containing module and
source occurrence rather than from field shape, traversal order, or a user-visible generated name.

#### Scenario: Construct a parameter type without naming it

- **WHEN** `foo` accepts `Person` and a caller writes `foo(.{ name: "Julia", age: 32 })`
- **THEN** the literal is checked as a direct `Person` construction using the parameter's expected type

#### Scenario: Reorder a contextual record literal

- **WHEN** a contextual literal supplies every visible `Person` field in a different source order
- **THEN** initializers run in source order and the value retains `Person`'s canonical declaration order

#### Scenario: Infer one local anonymous record

- **WHEN** `let args = .{ name: "Julia", age: 32 }` has no expected aggregate type and later flows to a generic parameter
- **THEN** the binding and call retain one anonymous nominal record identity with fields `name` then `age`

### Requirement: Anonymous aggregates are convenient but never structural

An anonymous tuple or record type SHALL be usable through the value produced at its literal
occurrence, including storage in a local binding, borrowing, field or position projection, and
inference as a concrete generic argument. It SHALL have no source type name and MUST NOT be
importable, explicitly named in a public contract, or converted to another nominal aggregate merely
because their members have equal labels, positions, or types.

Expected type SHALL flow into literals from an explicit binding type, parameter type, declared
return type, or another already-determined contextual contract. Expected type MUST NOT be invented
by unifying separate anonymous literal occurrences. Consequently, branches containing separate
uncontextualized literals SHALL have distinct result types, and equality or assignment between
separate same-shaped anonymous values SHALL fail nominal compatibility. Supplying one explicit
nominal expected type to the enclosing expression MAY contextually construct that same type in
every branch.

#### Scenario: Reject inferred branch unification

- **WHEN** an untyped conditional has a separate same-shaped record literal in each arm
- **THEN** the arms have different anonymous nominal identities and the conditional has no unified result type

#### Scenario: Admit explicitly contextual branches

- **WHEN** the same conditional is checked under an explicit `Person` expected type
- **THEN** both arms construct `Person` and the conditional result has that one nominal type

#### Scenario: Reject shape-based equality

- **WHEN** two local bindings are initialized by separate same-shaped anonymous record literals and compared
- **THEN** equality analysis rejects the incompatible nominal operand types rather than comparing by field shape

### Requirement: Tuples and records remain separate source concepts

Tuple positions SHALL be ordinal and record members SHALL be labeled. Silk MUST NOT infer labels
for positional elements, admit labeled tuple syntax, treat a record's source field order as tuple
positions, or establish compatibility between a tuple and a record. Future static reflection MAY
observe their declared positions or labels, but this capability SHALL NOT add field iteration,
compile-time reflection, variadic parameters, template parsing, or formatting behavior.

#### Scenario: Reject a labeled tuple

- **WHEN** source writes `(name: "Julia", age: 32)`
- **THEN** parsing or analysis rejects it and directs named aggregate construction to `.{ name: "Julia", age: 32 }`

#### Scenario: Keep formatting outside the aggregate contract

- **WHEN** a generic function receives an anonymous record argument
- **THEN** ordinary generic inference preserves the concrete aggregate type but no reflection or formatting operation is synthesized
