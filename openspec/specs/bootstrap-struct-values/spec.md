# bootstrap-struct-values Specification

## Purpose

Define complete runtime nominal struct values with field-visible construction, typed field access,
whole-value affine ownership, aggregate calls and returns, and deterministic cleanup.

## Requirements

### Requirement: Struct construction authority is field-based

Source MAY construct a source-declared nominal struct from any module when every supplied or
required initialized field is visible at the construction site. A private field SHALL preserve the
type's construction boundary; visibility of another field or the type name SHALL NOT grant access
to it. A declarationless opaque nominal type SHALL remain non-constructible.

Every literal SHALL initialize every required visible field exactly once. Initializers SHALL
evaluate in source order, while the complete value SHALL retain canonical declaration field order.
Unknown, duplicate, missing, inaccessible, or mistyped initializers SHALL remain independently
queryable and SHALL NOT create a partial value. A missing inaccessible field diagnostic SHALL NOT
reveal the hidden field's name or type.

#### Scenario: Construct from public fields

- **WHEN** an imported struct exposes all of its fields publicly
- **THEN** another module may construct it with named field initialization

#### Scenario: Preserve a private representation field

- **WHEN** one required field is private
- **THEN** external construction is rejected at that field and a public factory remains usable

#### Scenario: Preserve reordered initialization

- **WHEN** a literal supplies visible fields in an order different from their declaration order
- **THEN** expressions evaluate in source order and the complete value maps them into canonical declaration order

#### Scenario: Reject declarationless opaque construction

- **WHEN** source writes a literal for a nominal runtime type that has no source struct declaration
- **THEN** construction is rejected without granting it an invented zero-field constructor

### Requirement: Ordinary struct parameters infer from all supplied fields

Omitted ordinary generic arguments SHALL be inferred forward from all supplied field expressions using the same compatibility and conflict rules as function calls. Explicit type arguments SHALL form a prefix, and ambiguity or disagreement SHALL produce deterministic diagnostics.

#### Scenario: Infer one parameter from multiple fields

- **WHEN** `Pair<T>` is constructed with two fields that both resolve to `i32`
- **THEN** the constructed type is `Pair<i32>` without an explicit argument

#### Scenario: Diagnose conflicting fields

- **WHEN** two fields constrain the same omitted parameter to incompatible types
- **THEN** analysis reports both field constraints and does not choose by source order

### Requirement: Field projection is a canonical typed place

A field projection SHALL resolve its subject's nominal type and one field identity from that
struct's completed declaration. Projection SHALL associate left-to-right so nested fields remain a
chain of canonical place facts. A field SHALL be readable outside its defining module only when it
is public. Projecting a non-struct value, an unknown field, a private external field, or a subject
whose type is unavailable SHALL retain an explicit failed projection and exact cause.

#### Scenario: Read a scalar field

- **WHEN** `pair.left` projects an `i32` field from an available `Pair`
- **THEN** the projection identifies `Pair.left` canonically and produces an `i32` value without consuming `pair`

#### Scenario: Project through nested structs

- **WHEN** `token.span.start` follows two available nominal fields
- **THEN** both projection steps retain their canonical field identities and the final expression has the declared scalar type

#### Scenario: Refuse a private external field

- **WHEN** another module projects a default-private field of a public struct
- **THEN** the field candidate and use-site provenance remain visible but no readable place or value is produced

### Requirement: Bootstrap structs move only as whole values

Every user-defined struct value SHALL be move-only in this slice. Moving a whole struct SHALL
consume its source binding, and a later read or move SHALL be rejected. Reading a Copy scalar field
SHALL copy that field without consuming the owner. Moving an individual field out of a struct MUST
be rejected as a partial move; struct patterns, replacement operations, and user-declared `Copy`
remain outside this capability. A consuming let binding, call argument, or return of a bound struct
SHALL require explicit `move`; a fresh literal or call result MAY flow directly because it has no
source binding that could be copied accidentally.

#### Scenario: Move a complete struct through a call

- **WHEN** a caller passes `move token` to an owning nominal parameter
- **THEN** the callee receives the complete value and every later use of the caller's `token` is a use-after-move

#### Scenario: Return a newly constructed struct

- **WHEN** a factory returns a complete struct literal
- **THEN** the value crosses the return boundary as one owned nominal value without an intermediate partial state

#### Scenario: Refuse an implicit nominal copy

- **WHEN** a bound struct is passed to an owning parameter without `move`
- **THEN** ownership rejects the transfer and leaves the source binding live

#### Scenario: Refuse moving one nested field

- **WHEN** code attempts `move outer.inner`
- **THEN** ownership rejects the partial move while leaving `outer`'s whole-value state explicit

### Requirement: Struct cleanup follows declared ownership

A live owned struct SHALL clean up its fields in the language's declaration-defined cleanup order,
recursively using each field type's cleanup behavior, exactly once on every structured exit. A moved
whole value SHALL transfer that obligation to its destination and SHALL not be cleaned up at the
source. Empty and scalar-only structs SHALL still retain explicit complete cleanup facts even when
they require no runtime release operation.

#### Scenario: Transfer cleanup with a whole move

- **WHEN** a live struct binding is moved into another binding before return
- **THEN** only the destination owns the declaration-ordered field cleanup obligation at the exit

#### Scenario: Clean up a nested aggregate

- **WHEN** a live outer struct owns an inner struct at a structured exit
- **THEN** cleanup visits the outer fields and recursively the inner fields in their specified declaration-defined order exactly once

### Requirement: Contextual aggregate construction preserves nominal authority

Expected-type analysis SHALL resolve `(values...)` only to a named tuple and `.{ fields... }` only
to a source-declared named struct of matching aggregate kind. Contextual construction SHALL enforce
the target declaration's arity or field completeness, visibility, generic inference, member types,
and construction authority exactly as if its type name were written. It MUST NOT bypass a private
field, manufacture access to an opaque nominal, or use shape compatibility to select among types.

When no expected nominal aggregate is already determined, analysis SHALL create exactly one
anonymous nominal aggregate for the literal rather than searching visible declarations by shape.
Unknown, duplicate, missing, inaccessible, or incompatible members SHALL remain independently
queryable and SHALL produce no partial aggregate value.

#### Scenario: Preserve a private construction boundary

- **WHEN** an external caller passes `.{ ... }` to a function parameter whose struct type has one private required field
- **THEN** contextual construction is rejected without revealing or bypassing the hidden representation field

#### Scenario: Avoid shape-based type search

- **WHEN** two visible named structs have the same public fields and an uncontextualized record literal matches both shapes
- **THEN** analysis creates one anonymous nominal record and does not select either visible declaration

#### Scenario: Diagnose the expected tuple position

- **WHEN** a positional literal has the wrong arity or an incompatible element for its expected named tuple
- **THEN** analysis identifies the expected declaration and offending ordinal without producing a partial value
